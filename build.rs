use std::{
    env,
    fmt::Write,
    fs::read_to_string,
    path::{Path, PathBuf},
    process::Command,
};

use quote::ToTokens;
use std::fmt::Display;
use syn::export::Formatter;
use syn::{parse_file, FnArg, ImplItem, ImplItemMethod, Item, TraitItem, TraitItemMethod};

const TAB: &str = "    ";

pub fn main() {
    let stdlib_src = try_find_stdlib_path("libcore");
    if !stdlib_src.exists() {
        panic!(
            "can't load standard library from sysroot\n\
             {:?}\n\
             try running `rustup component add rust-src` or set `RUST_SRC_PATH`",
            stdlib_src,
        );
    }
    eprintln!("stdlib found {:?}", stdlib_src);

    generate_std_library_definitions(&stdlib_src);
}

fn try_find_stdlib_path(lib: &str) -> PathBuf {
    let manifest_path = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR env");

    if let Ok(path) = env::var("RUST_SRC_PATH") {
        return path.into();
    }

    let rustc_output = Command::new("rustc")
        .current_dir(manifest_path)
        .args(&["--print", "sysroot"])
        .output()
        .expect("run rustc");
    if !rustc_output.status.success() {
        panic!("failed to locate sysroot");
    }
    let stdout = String::from_utf8(rustc_output.stdout).expect("from utf8 path");
    let sysroot_path = Path::new(stdout.trim());

    sysroot_path.join(&format!("lib/rustlib/src/rust/src/{}", lib))
}

fn generate_std_library_definitions(stdlib_path: &PathBuf) {
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR env"));

    let libcore = parse_module("std", &stdlib_path.join("lib.rs"));
    //let libstd = parse_module("std", &stdlib_path.join("libstd"));

    let mut code = String::new();

    writeln!(code, "reflect::library! {{").expect("write generated code");

    writeln!(
        code,
        "{}",
        libcore.generate_extern_crate(1).expect("generating code")
    )
    .expect("write generated code");
    //writeln!(code, "{}", libstd.generate_extern_crate(2));

    writeln!(code, "}}").expect("write generated code");

    let path = out_dir.join("out.rs");
    std::fs::write(path, code).expect("write generated code");
}

fn parse_module(mod_name: &str, module_path: &PathBuf) -> Mod {
    let src = read_to_string(module_path).expect("read module");
    let syntax = parse_file(&src).expect("parse file");

    Mod {
        name: mod_name.to_owned(),
        items: collect_items(&module_path, &syntax.items),
    }
}

fn collect_items(module_path: &PathBuf, items: &[Item]) -> Vec<ModItem> {
    items
        .iter()
        .filter_map(|item| match item {
            Item::Enum(e) => {
                let item = ModItem::Type {
                    name: e.ident.to_string(),
                };

                Some(item)
            }

            Item::Mod(m) => {
                if m.ident == "tests" {
                    return None;
                }

                let mod_path = module_path
                    .parent()
                    .expect("has parent")
                    .join(&format!("{}.rs", m.ident));
                let mod_name = m.ident.to_string();
                let m = if mod_path.exists() {
                    parse_module(&mod_name, &mod_path)
                } else {
                    let mod_rs_path = module_path
                        .parent()
                        .expect("has parent")
                        .join(m.ident.to_string())
                        .join("mod.rs");
                    if mod_rs_path.exists() {
                        parse_module(&mod_name, &mod_rs_path)
                    } else if let Some((_, items)) = &m.content {
                        Mod {
                            name: mod_name,
                            items: collect_items(&mod_path, items),
                        }
                    } else {
                        return None;
                    }
                };

                Some(ModItem::Mod(m))
            }

            Item::Struct(s) => {
                let item = ModItem::Type {
                    name: s.ident.to_string(),
                };

                Some(item)
            }

            Item::Trait(t) => {
                let items = t
                    .items
                    .iter()
                    .filter_map(|i| {
                        if let TraitItem::Method(m) = i {
                            // FIXME: methods returning Self are not supported by `library!` macro
                            if m.sig.output.to_token_stream().to_string().contains("Self") {
                                return None;
                            }

                            // FIXME: methods with generic parameters are not supported by `library!` macro
                            if m.sig.inputs.to_token_stream().to_string().contains('<') {
                                return None;
                            }

                            Some(make_trait_method(&m))
                        } else {
                            None
                        }
                    })
                    .collect();

                let t = ModItem::Trait {
                    name: t.ident.to_string(),
                    items,
                };

                Some(t)
            }

            Item::Type(ty) => {
                let item = ModItem::Type {
                    name: ty.ident.to_string(),
                };

                Some(item)
            }

            Item::Impl(i) => {
                let type_name = make_type_name(&i.self_ty);
                // Generic/array impls are not supported by `library!` macro
                if type_name.contains('<') || type_name.contains('[') || type_name.contains('*') {
                    return None;
                }

                if i.trait_.is_some() {
                    return None;
                };

                let items = i
                    .items
                    .iter()
                    .filter_map(|i| {
                        if let ImplItem::Method(m) = i {
                            // FIXME: methods returning Self are not supported by `library!` macro
                            if m.sig.output.to_token_stream().to_string().contains("Self") {
                                return None;
                            }

                            // FIXME: methods with generic parameters are not supported by `library!` macro
                            if m.sig.inputs.to_token_stream().to_string().contains('<') {
                                return None;
                            }

                            Some(make_impl_method(&m))
                        } else {
                            None
                        }
                    })
                    .collect();

                let i = ModItem::Impl {
                    target: type_name,
                    items,
                };

                Some(i)
            }

            _ => None,
        })
        .filter(should_keep_item)
        .collect()
}

fn should_keep_item(item: &ModItem) -> bool {
    match item {
        ModItem::Type { .. } => true,
        ModItem::Trait { .. } => true,
        ModItem::Impl { items, .. } => !items.is_empty(),
        ModItem::Mod(m) => !m.items.is_empty(),
    }
}

fn make_type_name(ty: &Box<syn::Type>) -> String {
    ty.to_token_stream().to_string()
}

fn make_trait_method(m: &TraitItemMethod) -> Method {
    let output = m.sig.output.to_token_stream();
    let inputs = m
        .sig
        .inputs
        .iter()
        .map(|i| match i {
            FnArg::Receiver(r) => r.to_token_stream().to_string(),
            FnArg::Typed(ty) => ty.ty.to_token_stream().to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    Method {
        name: m.sig.ident.to_string(),
        tokens: format!("fn {}({}) {};", m.sig.ident, inputs, output),
    }
}

fn make_impl_method(m: &ImplItemMethod) -> Method {
    let output = m.sig.output.to_token_stream();
    let inputs = m
        .sig
        .inputs
        .iter()
        .map(|i| match i {
            FnArg::Receiver(r) => r.to_token_stream().to_string(),
            FnArg::Typed(ty) => ty.ty.to_token_stream().to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");

    Method {
        name: m.sig.ident.to_string(),
        tokens: format!("fn {}({}) {};", m.sig.ident, inputs, output),
    }
}

#[derive(Debug)]
struct Mod {
    name: String,
    items: Vec<ModItem>,
}

impl Mod {
    pub fn generate_code(&self, depth: usize) -> Result<String, std::fmt::Error> {
        let mut code = String::new();

        write!(code, "{}", TAB.repeat(depth))?;
        writeln!(code, "mod {} {{", self.name)?;

        for item in &self.items {
            write!(code, "{}", item.generate_code(depth + 1)?)?;
        }

        write!(code, "{}", TAB.repeat(depth))?;
        writeln!(code, "}}")?;
        writeln!(code, "")?;

        Ok(code)
    }

    pub fn generate_extern_crate(&self, depth: usize) -> Result<String, std::fmt::Error> {
        let mut code = String::new();

        write!(code, "{}", TAB.repeat(depth))?;
        writeln!(code, "extern crate {} {{", self.name)?;

        for item in &self.items {
            write!(code, "{}", item.generate_code(depth + 1)?)?;
        }

        write!(code, "{}", TAB.repeat(depth))?;
        writeln!(code, "}}")?;
        writeln!(code, "")?;

        Ok(code)
    }
}

#[derive(Debug)]
enum ModItem {
    Type { name: String },

    Trait { name: String, items: Vec<Method> },

    Impl { target: String, items: Vec<Method> },

    Mod(Mod),
}

impl ModItem {
    pub fn generate_code(&self, depth: usize) -> Result<String, std::fmt::Error> {
        let mut code = String::new();

        match self {
            ModItem::Type { name } => {
                write!(code, "{}", TAB.repeat(depth))?;
                writeln!(code, "type {};", name)?;
            }

            ModItem::Trait { name, items } => {
                write!(code, "{}", TAB.repeat(depth))?;
                writeln!(code, "trait {} {{", name)?;

                for item in items {
                    write!(code, "{}", TAB.repeat(depth + 1))?;
                    writeln!(code, "{}", item.to_string())?;
                }

                write!(code, "{}", TAB.repeat(depth))?;
                writeln!(code, "}}")?;
            }

            ModItem::Impl { target, items } => {
                write!(code, "{}", TAB.repeat(depth))?;
                writeln!(code, "impl {} {{", target)?;

                for item in items {
                    write!(code, "{}", TAB.repeat(depth + 1))?;
                    writeln!(code, "{}", item.to_string())?;
                }

                write!(code, "{}", TAB.repeat(depth))?;
                writeln!(code, "}}")?;
            }

            ModItem::Mod(module) => {
                write!(code, "{}", module.generate_code(depth)?)?;
            }
        }

        Ok(code)
    }
}

#[derive(Debug)]
struct Method {
    name: String,
    tokens: String,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.tokens)
    }
}
