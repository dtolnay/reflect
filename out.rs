reflect::library! {
    extern crate std {
        mod num {
            type Wrapping;
            mod flt2dec {
                mod decoder {
                    type Decoded;
                    type FullDecoded;
                    trait DecodableFloat {
                    }
                }

                type Part;
                type Formatted;
                type Sign;
            }

            mod dec2flt {
                mod algorithm {
                    mod fpu_precision {
                        type FPUControlWord;
                    }

                }

                mod rawfp {
                    type Unpacked;
                    trait RawFloat {
                        fn classify(self) -> FpCategory;
                        fn integer_decode(self) -> ( u64 , i16 , i8 );
                        fn unpack(self) -> Unpacked;
                    }
                }

                mod parse {
                    type Sign;
                    type Decimal;
                    type ParseResult;
                }

                type ParseFloatError;
                type FloatErrorKind;
                impl ParseFloatError {
                    fn __description(& self) -> & str;
                }
            }

            mod bignum {
                trait FullOps {
                }
                type Digit32;
            }

            mod diy_float {
                type Fp;
                impl Fp {
                    fn mul(& self, & Fp) -> Fp;
                    fn normalize(& self) -> Fp;
                    fn normalize_to(& self, i16) -> Fp;
                }
            }

            impl u8 {
                fn is_ascii(& self) -> bool;
                fn to_ascii_uppercase(& self) -> u8;
                fn to_ascii_lowercase(& self) -> u8;
                fn eq_ignore_ascii_case(& self, & u8) -> bool;
                fn make_ascii_uppercase(& mut self) ;
                fn make_ascii_lowercase(& mut self) ;
                fn is_ascii_alphabetic(& self) -> bool;
                fn is_ascii_uppercase(& self) -> bool;
                fn is_ascii_lowercase(& self) -> bool;
                fn is_ascii_alphanumeric(& self) -> bool;
                fn is_ascii_digit(& self) -> bool;
                fn is_ascii_hexdigit(& self) -> bool;
                fn is_ascii_punctuation(& self) -> bool;
                fn is_ascii_graphic(& self) -> bool;
                fn is_ascii_whitespace(& self) -> bool;
                fn is_ascii_control(& self) -> bool;
            }
            type FpCategory;
            type TryFromIntError;
            impl TryFromIntError {
                fn __description(& self) -> & str;
            }
            trait FromStrRadixHelper {
            }
            type ParseIntError;
            type IntErrorKind;
            impl ParseIntError {
                fn kind(& self) -> & IntErrorKind;
                fn __description(& self) -> & str;
            }
        }

        mod mem {
            mod manually_drop {
                type ManuallyDrop;
            }

            type Discriminant;
        }

        mod ptr {
            mod non_null {
                type NonNull;
            }

            mod unique {
                type Unique;
            }

            type FatPtr;
        }

        mod marker {
            trait Send {
            }
            trait Sized {
            }
            trait Unsize {
            }
            trait Copy {
            }
            trait Sync {
            }
            type PhantomData;
            trait Freeze {
            }
            trait Unpin {
            }
            type PhantomPinned;
        }

        mod ops {
            mod arith {
                trait Add {
                }
                trait Sub {
                }
                trait Mul {
                }
                trait Div {
                }
                trait Rem {
                }
                trait Neg {
                }
                trait AddAssign {
                    fn add_assign(& mut self, Rhs) ;
                }
                trait SubAssign {
                    fn sub_assign(& mut self, Rhs) ;
                }
                trait MulAssign {
                    fn mul_assign(& mut self, Rhs) ;
                }
                trait DivAssign {
                    fn div_assign(& mut self, Rhs) ;
                }
                trait RemAssign {
                    fn rem_assign(& mut self, Rhs) ;
                }
            }

            mod bit {
                trait Not {
                }
                trait BitAnd {
                }
                trait BitOr {
                }
                trait BitXor {
                }
                trait Shl {
                }
                trait Shr {
                }
                trait BitAndAssign {
                    fn bitand_assign(& mut self, Rhs) ;
                }
                trait BitOrAssign {
                    fn bitor_assign(& mut self, Rhs) ;
                }
                trait BitXorAssign {
                    fn bitxor_assign(& mut self, Rhs) ;
                }
                trait ShlAssign {
                    fn shl_assign(& mut self, Rhs) ;
                }
                trait ShrAssign {
                    fn shr_assign(& mut self, Rhs) ;
                }
            }

            mod deref {
                trait Deref {
                }
                trait DerefMut {
                }
                trait Receiver {
                }
            }

            mod drop {
                trait Drop {
                    fn drop(& mut self) ;
                }
            }

            mod function {
                trait Fn {
                }
                trait FnMut {
                }
                trait FnOnce {
                }
            }

            mod generator {
                type GeneratorState;
                trait Generator {
                }
            }

            mod index {
                trait Index {
                }
                trait IndexMut {
                }
            }

            mod range {
                type RangeFull;
                type Range;
                type RangeFrom;
                type RangeTo;
                type RangeInclusive;
                trait RangeInclusiveEquality {
                }
                type RangeToInclusive;
                type Bound;
                trait RangeBounds {
                    fn start_bound(& self) -> Bound < & T >;
                    fn end_bound(& self) -> Bound < & T >;
                    fn contains(& self, & U) -> bool;
                }
            }

            mod unsize {
                trait CoerceUnsized {
                }
                trait DispatchFromDyn {
                }
            }

        }

        mod cmp {
            trait PartialEq {
                fn eq(& self, & Rhs) -> bool;
                fn ne(& self, & Rhs) -> bool;
            }
            trait Eq {
                fn assert_receiver_is_total_eq(& self) ;
            }
            type AssertParamIsEq;
            type Ordering;
            impl Ordering {
                fn reverse(self) -> Ordering;
                fn then(self, Ordering) -> Ordering;
                fn then_with(self, F) -> Ordering;
            }
            type Reverse;
            trait Ord {
                fn cmp(& self, & Self) -> Ordering;
            }
            trait PartialOrd {
                fn partial_cmp(& self, & Rhs) -> Option < Ordering >;
                fn lt(& self, & Rhs) -> bool;
                fn le(& self, & Rhs) -> bool;
                fn gt(& self, & Rhs) -> bool;
                fn ge(& self, & Rhs) -> bool;
            }
        }

        mod clone {
            trait Clone {
                fn clone_from(& mut self, & Self) ;
            }
            type AssertParamIsClone;
            type AssertParamIsCopy;
        }

        mod default {
            trait Default {
            }
        }

        mod convert {
            trait AsRef {
                fn as_ref(& self) -> & T;
            }
            trait AsMut {
                fn as_mut(& mut self) -> & mut T;
            }
            trait Into {
                fn into(self) -> T;
            }
            trait From {
            }
            trait TryInto {
            }
            trait TryFrom {
            }
            type Infallible;
        }

        mod borrow {
            trait Borrow {
                fn borrow(& self) -> & Borrowed;
            }
            trait BorrowMut {
                fn borrow_mut(& mut self) -> & mut Borrowed;
            }
        }

        mod any {
            trait Any {
                fn type_id(& self) -> TypeId;
            }
            impl dyn Any {
                fn is(& self) -> bool;
                fn downcast_ref(& self) -> Option < & T >;
                fn downcast_mut(& mut self) -> Option < & mut T >;
            }
            impl dyn Any + Send {
                fn is(& self) -> bool;
                fn downcast_ref(& self) -> Option < & T >;
                fn downcast_mut(& mut self) -> Option < & mut T >;
            }
            impl dyn Any + Send + Sync {
                fn is(& self) -> bool;
                fn downcast_ref(& self) -> Option < & T >;
                fn downcast_mut(& mut self) -> Option < & mut T >;
            }
            type TypeId;
            impl TypeId {
                fn of() -> TypeId;
            }
        }

        mod array {
            trait FixedSizeArray {
                fn as_slice(& self) -> & [ T ];
                fn as_mut_slice(& mut self) -> & mut [ T ];
            }
            type TryFromSliceError;
            impl TryFromSliceError {
                fn __description(& self) -> & str;
            }
            trait LengthAtMost32 {
            }
        }

        mod ascii {
            type EscapeDefault;
        }

        mod sync {
            mod atomic {
                type AtomicBool;
                type AtomicPtr;
                type Ordering;
                impl AtomicBool {
                    fn new(bool) -> AtomicBool;
                    fn get_mut(& mut self) -> & mut bool;
                    fn into_inner(self) -> bool;
                    fn load(& self, Ordering) -> bool;
                    fn store(& self, bool, Ordering) ;
                    fn swap(& self, bool, Ordering) -> bool;
                    fn compare_and_swap(& self, bool, bool, Ordering) -> bool;
                    fn compare_exchange(& self, bool, bool, Ordering, Ordering) -> Result < bool , bool >;
                    fn compare_exchange_weak(& self, bool, bool, Ordering, Ordering) -> Result < bool , bool >;
                    fn fetch_and(& self, bool, Ordering) -> bool;
                    fn fetch_nand(& self, bool, Ordering) -> bool;
                    fn fetch_or(& self, bool, Ordering) -> bool;
                    fn fetch_xor(& self, bool, Ordering) -> bool;
                }
            }

        }

        mod cell {
            type Cell;
            type RefCell;
            type BorrowError;
            type BorrowMutError;
            type BorrowFlag;
            type BorrowRef;
            type Ref;
            type BorrowRefMut;
            type RefMut;
            type UnsafeCell;
        }

        mod char {
            mod convert {
                type ParseCharError;
                impl ParseCharError {
                    fn __description(& self) -> & str;
                }
                type CharErrorKind;
                type CharTryFromError;
            }

            mod decode {
                type DecodeUtf16;
                type DecodeUtf16Error;
                impl DecodeUtf16Error {
                    fn unpaired_surrogate(& self) -> u16;
                }
            }

            mod methods {
                impl char {
                    fn is_digit(self, u32) -> bool;
                    fn to_digit(self, u32) -> Option < u32 >;
                    fn escape_unicode(self) -> EscapeUnicode;
                    fn escape_debug_ext(self, bool) -> EscapeDebug;
                    fn escape_debug(self) -> EscapeDebug;
                    fn escape_default(self) -> EscapeDefault;
                    fn len_utf8(self) -> usize;
                    fn len_utf16(self) -> usize;
                    fn encode_utf8(self, & mut [ u8 ]) -> & mut str;
                    fn encode_utf16(self, & mut [ u16 ]) -> & mut [ u16 ];
                    fn is_alphabetic(self) -> bool;
                    fn is_xid_start(self) -> bool;
                    fn is_xid_continue(self) -> bool;
                    fn is_lowercase(self) -> bool;
                    fn is_uppercase(self) -> bool;
                    fn is_whitespace(self) -> bool;
                    fn is_alphanumeric(self) -> bool;
                    fn is_control(self) -> bool;
                    fn is_grapheme_extended(self) -> bool;
                    fn is_numeric(self) -> bool;
                    fn to_lowercase(self) -> ToLowercase;
                    fn to_uppercase(self) -> ToUppercase;
                    fn is_ascii(& self) -> bool;
                    fn to_ascii_uppercase(& self) -> char;
                    fn to_ascii_lowercase(& self) -> char;
                    fn eq_ignore_ascii_case(& self, & char) -> bool;
                    fn make_ascii_uppercase(& mut self) ;
                    fn make_ascii_lowercase(& mut self) ;
                    fn is_ascii_alphabetic(& self) -> bool;
                    fn is_ascii_uppercase(& self) -> bool;
                    fn is_ascii_lowercase(& self) -> bool;
                    fn is_ascii_alphanumeric(& self) -> bool;
                    fn is_ascii_digit(& self) -> bool;
                    fn is_ascii_hexdigit(& self) -> bool;
                    fn is_ascii_punctuation(& self) -> bool;
                    fn is_ascii_graphic(& self) -> bool;
                    fn is_ascii_whitespace(& self) -> bool;
                    fn is_ascii_control(& self) -> bool;
                }
            }

            type EscapeUnicode;
            type EscapeUnicodeState;
            type EscapeDefault;
            type EscapeDefaultState;
            type EscapeDebug;
            type ToLowercase;
            type ToUppercase;
            type CaseMappingIter;
            impl CaseMappingIter {
                fn new([ char ; 3 ]) -> CaseMappingIter;
            }
        }

        mod panic {
            type PanicInfo;
            type Location;
            trait BoxMeUp {
                fn box_me_up(& mut self) -> * mut ( dyn Any + Send );
                fn get(& mut self) -> & ( dyn Any + Send );
            }
        }

        mod pin {
            type Pin;
        }

        mod iter {
            mod range {
                trait Step {
                    fn steps_between(& Self, & Self) -> Option < usize >;
                }
            }

            mod sources {
                type Repeat;
                type RepeatWith;
                type Empty;
                type Once;
                type OnceWith;
                type FromFn;
                type Successors;
            }

            mod traits {
                mod iterator {
                    trait Iterator {
                        fn size_hint(& self) -> ( usize , Option < usize > );
                        fn count(self) -> usize;
                        fn for_each(self, F) ;
                        fn collect(self) -> B;
                        fn partition(self, F) -> ( B , B );
                        fn partition_in_place(mut self, P) -> usize;
                        fn is_partitioned(mut self, P) -> bool;
                        fn try_fold(& mut self, B, F) -> R;
                        fn try_for_each(& mut self, F) -> R;
                        fn fold(mut self, B, F) -> B;
                        fn all(& mut self, F) -> bool;
                        fn any(& mut self, F) -> bool;
                        fn find_map(& mut self, F) -> Option < B >;
                        fn position(& mut self, P) -> Option < usize >;
                        fn rposition(& mut self, P) -> Option < usize >;
                        fn unzip(self) -> ( FromA , FromB );
                        fn sum(self) -> S;
                        fn product(self) -> P;
                        fn cmp(mut self, I) -> Ordering;
                        fn partial_cmp(mut self, I) -> Option < Ordering >;
                        fn eq(mut self, I) -> bool;
                        fn ne(self, I) -> bool;
                        fn lt(self, I) -> bool;
                        fn le(self, I) -> bool;
                        fn gt(self, I) -> bool;
                        fn ge(self, I) -> bool;
                        fn is_sorted(self) -> bool;
                        fn is_sorted_by(mut self, F) -> bool;
                        fn is_sorted_by_key(self, F) -> bool;
                    }
                }

                mod double_ended {
                    trait DoubleEndedIterator {
                        fn try_rfold(& mut self, B, F) -> R;
                        fn rfold(mut self, B, F) -> B;
                    }
                }

                mod exact_size {
                    trait ExactSizeIterator {
                        fn len(& self) -> usize;
                        fn is_empty(& self) -> bool;
                    }
                }

                mod collect {
                    trait FromIterator {
                    }
                    trait IntoIterator {
                    }
                    trait Extend {
                        fn extend(& mut self, T) ;
                    }
                }

                mod accum {
                    trait Sum {
                    }
                    trait Product {
                    }
                }

                mod marker {
                    trait FusedIterator {
                    }
                    trait TrustedLen {
                    }
                }

            }

            mod adapters {
                mod chain {
                    type Chain;
                    type ChainState;
                }

                mod flatten {
                    type FlatMap;
                    type Flatten;
                    type FlattenCompat;
                }

                mod zip {
                    type Zip;
                    trait ZipImpl {
                        fn size_hint(& self) -> ( usize , Option < usize > );
                    }
                    trait TrustedRandomAccess {
                        fn may_have_side_effect() -> bool;
                    }
                }

                type Rev;
                type Copied;
                type Cloned;
                type Cycle;
                type StepBy;
                type Map;
                type Filter;
                type FilterMap;
                type Enumerate;
                type Peekable;
                type SkipWhile;
                type TakeWhile;
                type Skip;
                type Take;
                type Scan;
                type Fuse;
                type Inspect;
                type ResultShunt;
            }

            type LoopState;
        }

        mod option {
            type Option;
            type Item;
            type Iter;
            type IterMut;
            type IntoIter;
            type NoneError;
        }

        mod raw {
            type TraitObject;
        }

        mod result {
            type Result;
            type Iter;
            type IterMut;
            type IntoIter;
        }

        mod ffi {
            type c_void;
            type VaListImpl;
            type VaListImpl;
            type VaListImpl;
            type VaListImpl;
            type VaListImpl;
            type VaList;
            mod sealed_trait {
                trait VaArgSafe {
                }
            }

        }

        mod slice {
            mod sort {
                type CopyOnDrop;
            }

            mod private_slice_index {
                trait Sealed {
                }
            }

            trait SliceIndex {
            }
            type Iter;
            type IterMut;
            trait SplitIter {
            }
            type Split;
            type SplitMut;
            type RSplit;
            type RSplitMut;
            type GenericSplitN;
            type SplitN;
            type RSplitN;
            type SplitNMut;
            type RSplitNMut;
            type Windows;
            type Chunks;
            type ChunksMut;
            type ChunksExact;
            type ChunksExactMut;
            type RChunks;
            type RChunksMut;
            type RChunksExact;
            type RChunksExactMut;
            trait SlicePartialEq {
                fn equal(& self, & [ B ]) -> bool;
                fn not_equal(& self, & [ B ]) -> bool;
            }
            trait SlicePartialOrd {
                fn partial_compare(& self, & [ B ]) -> Option < Ordering >;
            }
            trait SliceOrd {
                fn compare(& self, & [ B ]) -> Ordering;
            }
            trait BytewiseEquality {
            }
            trait SliceContains {
                fn slice_contains(& self, & [ Self ]) -> bool;
            }
        }

        mod str {
            mod pattern {
                trait Pattern {
                    fn is_contained_in(self, & 'a str) -> bool;
                    fn is_prefix_of(self, & 'a str) -> bool;
                    fn is_suffix_of(self, & 'a str) -> bool;
                }
                type SearchStep;
                trait Searcher {
                    fn haystack(& self) -> & 'a str;
                    fn next(& mut self) -> SearchStep;
                    fn next_match(& mut self) -> Option < ( usize , usize ) >;
                    fn next_reject(& mut self) -> Option < ( usize , usize ) >;
                }
                trait ReverseSearcher {
                    fn next_back(& mut self) -> SearchStep;
                    fn next_match_back(& mut self) -> Option < ( usize , usize ) >;
                    fn next_reject_back(& mut self) -> Option < ( usize , usize ) >;
                }
                trait DoubleEndedSearcher {
                }
                type CharSearcher;
                trait MultiCharEq {
                    fn matches(& mut self, char) -> bool;
                }
                type MultiCharEqPattern;
                type MultiCharEqSearcher;
                type CharSliceSearcher;
                type CharPredicateSearcher;
                type StrSearcher;
                type StrSearcherImpl;
                type EmptyNeedle;
                type TwoWaySearcher;
                impl TwoWaySearcher {
                    fn new(& [ u8 ], usize) -> TwoWaySearcher;
                    fn byteset_create(& [ u8 ]) -> u64;
                    fn byteset_contains(& self, u8) -> bool;
                    fn next(& mut self, & [ u8 ], & [ u8 ], bool) -> S :: Output;
                    fn next_back(& mut self, & [ u8 ], & [ u8 ], bool) -> S :: Output;
                    fn maximal_suffix(& [ u8 ], bool) -> ( usize , usize );
                    fn reverse_maximal_suffix(& [ u8 ], usize, bool) -> usize;
                }
                trait TwoWayStrategy {
                    fn use_early_reject() -> bool;
                }
                type MatchOnly;
                type RejectAndMatch;
            }

            mod lossy {
                type Utf8Lossy;
                impl Utf8Lossy {
                    fn from_str(& str) -> & Utf8Lossy;
                    fn from_bytes(& [ u8 ]) -> & Utf8Lossy;
                    fn chunks(& self) -> Utf8LossyChunksIter < '_ >;
                }
                type Utf8LossyChunksIter;
                type Utf8LossyChunk;
            }

            trait FromStr {
            }
            type ParseBoolError;
            type Utf8Error;
            impl Utf8Error {
                fn valid_up_to(& self) -> usize;
                fn error_len(& self) -> Option < usize >;
            }
            type Chars;
            type CharIndices;
            type Bytes;
            type SplitInternal;
            type SplitNInternal;
            type MatchIndicesInternal;
            type MatchesInternal;
            type Lines;
            type LinesAny;
            impl str {
                fn len(& self) -> usize;
                fn is_empty(& self) -> bool;
                fn is_char_boundary(& self, usize) -> bool;
                fn as_bytes(& self) -> & [ u8 ];
                fn as_bytes_mut(& mut self) -> & mut [ u8 ];
                fn as_ptr(& self) -> * const u8;
                fn as_mut_ptr(& mut self) -> * mut u8;
                fn get(& self, I) -> Option < & I :: Output >;
                fn get_mut(& mut self, I) -> Option < & mut I :: Output >;
                fn get_unchecked(& self, I) -> & I :: Output;
                fn get_unchecked_mut(& mut self, I) -> & mut I :: Output;
                fn slice_unchecked(& self, usize, usize) -> & str;
                fn slice_mut_unchecked(& mut self, usize, usize) -> & mut str;
                fn split_at(& self, usize) -> ( & str , & str );
                fn split_at_mut(& mut self, usize) -> ( & mut str , & mut str );
                fn chars(& self) -> Chars < '_ >;
                fn char_indices(& self) -> CharIndices < '_ >;
                fn bytes(& self) -> Bytes < '_ >;
                fn split_whitespace(& self) -> SplitWhitespace < '_ >;
                fn split_ascii_whitespace(& self) -> SplitAsciiWhitespace < '_ >;
                fn lines(& self) -> Lines < '_ >;
                fn lines_any(& self) -> LinesAny < '_ >;
                fn encode_utf16(& self) -> EncodeUtf16 < '_ >;
                fn contains(& 'a self, P) -> bool;
                fn starts_with(& 'a self, P) -> bool;
                fn ends_with(& 'a self, P) -> bool;
                fn find(& 'a self, P) -> Option < usize >;
                fn rfind(& 'a self, P) -> Option < usize >;
                fn split(& 'a self, P) -> Split < 'a , P >;
                fn rsplit(& 'a self, P) -> RSplit < 'a , P >;
                fn split_terminator(& 'a self, P) -> SplitTerminator < 'a , P >;
                fn rsplit_terminator(& 'a self, P) -> RSplitTerminator < 'a , P >;
                fn splitn(& 'a self, usize, P) -> SplitN < 'a , P >;
                fn rsplitn(& 'a self, usize, P) -> RSplitN < 'a , P >;
                fn matches(& 'a self, P) -> Matches < 'a , P >;
                fn rmatches(& 'a self, P) -> RMatches < 'a , P >;
                fn match_indices(& 'a self, P) -> MatchIndices < 'a , P >;
                fn rmatch_indices(& 'a self, P) -> RMatchIndices < 'a , P >;
                fn trim(& self) -> & str;
                fn trim_start(& self) -> & str;
                fn trim_end(& self) -> & str;
                fn trim_left(& self) -> & str;
                fn trim_right(& self) -> & str;
                fn trim_matches(& 'a self, P) -> & 'a str;
                fn trim_start_matches(& 'a self, P) -> & 'a str;
                fn trim_end_matches(& 'a self, P) -> & 'a str;
                fn trim_left_matches(& 'a self, P) -> & 'a str;
                fn trim_right_matches(& 'a self, P) -> & 'a str;
                fn parse(& self) -> Result < F , F :: Err >;
                fn is_ascii(& self) -> bool;
                fn eq_ignore_ascii_case(& self, & str) -> bool;
                fn make_ascii_uppercase(& mut self) ;
                fn make_ascii_lowercase(& mut self) ;
                fn escape_debug(& self) -> EscapeDebug < '_ >;
                fn escape_default(& self) -> EscapeDefault < '_ >;
                fn escape_unicode(& self) -> EscapeUnicode < '_ >;
            }
            type SplitWhitespace;
            type SplitAsciiWhitespace;
            type EncodeUtf16;
            type EscapeDebug;
            type EscapeDefault;
            type EscapeUnicode;
        }

        mod hash {
            mod sip {
                type SipHasher13;
                type SipHasher24;
                type SipHasher;
                type Hasher;
                type State;
                impl SipHasher {
                    fn new() -> SipHasher;
                    fn new_with_keys(u64, u64) -> SipHasher;
                }
                impl SipHasher13 {
                    fn new() -> SipHasher13;
                    fn new_with_keys(u64, u64) -> SipHasher13;
                }
                trait Sip {
                    fn c_rounds(& mut State) ;
                    fn d_rounds(& mut State) ;
                }
                type Sip13Rounds;
                type Sip24Rounds;
            }

            trait Hash {
                fn hash(& self, & mut H) ;
                fn hash_slice(& [ Self ], & mut H) ;
            }
            trait Hasher {
                fn finish(& self) -> u64;
                fn write(& mut self, & [ u8 ]) ;
                fn write_u8(& mut self, u8) ;
                fn write_u16(& mut self, u16) ;
                fn write_u32(& mut self, u32) ;
                fn write_u64(& mut self, u64) ;
                fn write_u128(& mut self, u128) ;
                fn write_usize(& mut self, usize) ;
                fn write_i8(& mut self, i8) ;
                fn write_i16(& mut self, i16) ;
                fn write_i32(& mut self, i32) ;
                fn write_i64(& mut self, i64) ;
                fn write_i128(& mut self, i128) ;
                fn write_isize(& mut self, isize) ;
            }
            trait BuildHasher {
            }
            type BuildHasherDefault;
        }

        mod fmt {
            mod num {
                trait Int {
                    fn to_u8(& self) -> u8;
                    fn to_u16(& self) -> u16;
                    fn to_u32(& self) -> u32;
                    fn to_u64(& self) -> u64;
                    fn to_u128(& self) -> u128;
                }
                trait GenericRadix {
                    fn digit(u8) -> u8;
                }
                type Binary;
                type Octal;
                type LowerHex;
                type UpperHex;
            }

            mod builders {
                type PadAdapter;
                type PadAdapterState;
                type DebugStruct;
                type DebugTuple;
                type DebugInner;
                type DebugSet;
                type DebugList;
                type DebugMap;
            }

            type Alignment;
            type Result;
            type Error;
            trait Write {
                fn write_str(& mut self, & str) -> Result;
                fn write_char(& mut self, char) -> Result;
            }
            type Formatter;
            type Void;
            type ArgumentV1;
            type FlagV1;
            type Arguments;
            trait Debug {
            }
            trait Display {
            }
            trait Octal {
            }
            trait Binary {
            }
            trait LowerHex {
            }
            trait UpperHex {
            }
            trait Pointer {
            }
            trait LowerExp {
            }
            trait UpperExp {
            }
            type PostPadding;
            impl PostPadding {
                fn new(char, usize) -> PostPadding;
                fn write(self, & mut dyn Write) -> Result;
            }
        }

        mod time {
            type Duration;
            impl Duration {
                fn new(u64, u32) -> Duration;
                fn from_secs(u64) -> Duration;
                fn from_millis(u64) -> Duration;
                fn from_micros(u64) -> Duration;
                fn from_nanos(u64) -> Duration;
                fn as_secs(& self) -> u64;
                fn subsec_millis(& self) -> u32;
                fn subsec_micros(& self) -> u32;
                fn subsec_nanos(& self) -> u32;
                fn as_millis(& self) -> u128;
                fn as_micros(& self) -> u128;
                fn as_nanos(& self) -> u128;
                fn checked_add(self, Duration) -> Option < Duration >;
                fn checked_sub(self, Duration) -> Option < Duration >;
                fn checked_mul(self, u32) -> Option < Duration >;
                fn checked_div(self, u32) -> Option < Duration >;
                fn as_secs_f64(& self) -> f64;
                fn as_secs_f32(& self) -> f32;
                fn from_secs_f64(f64) -> Duration;
                fn from_secs_f32(f32) -> Duration;
                fn mul_f64(self, f64) -> Duration;
                fn mul_f32(self, f32) -> Duration;
                fn div_f64(self, f64) -> Duration;
                fn div_f32(self, f32) -> Duration;
                fn div_duration_f64(self, Duration) -> f64;
                fn div_duration_f32(self, Duration) -> f32;
            }
        }

        mod unicode {
            mod bool_trie {
                type BoolTrie;
                impl BoolTrie {
                    fn lookup(& self, char) -> bool;
                }
                type SmallBoolTrie;
                impl SmallBoolTrie {
                    fn lookup(& self, char) -> bool;
                }
            }

            mod version {
                type UnicodeVersion;
            }

        }

        mod future {
            mod future {
                trait Future {
                }
            }

        }

        mod task {
            mod poll {
                type Poll;
            }

            mod wake {
                type RawWaker;
                impl RawWaker {
                    fn new(* const ( ), & 'static RawWakerVTable) -> RawWaker;
                }
                type RawWakerVTable;
                type Context;
                type Waker;
                impl Waker {
                    fn wake(self) ;
                    fn wake_by_ref(& self) ;
                    fn will_wake(& self, & Waker) -> bool;
                    fn from_raw(RawWaker) -> Waker;
                }
            }

        }

        mod alloc {
            type Excess;
            type Layout;
            impl Layout {
                fn size(& self) -> usize;
                fn align(& self) -> usize;
                fn padding_needed_for(& self, usize) -> usize;
                fn pad_to_align(& self) -> Result < Layout , LayoutErr >;
            }
            type LayoutErr;
            type AllocErr;
            type CannotReallocInPlace;
            impl CannotReallocInPlace {
                fn description(& self) -> & str;
            }
            trait GlobalAlloc {
                fn alloc(& self, Layout) -> * mut u8;
                fn dealloc(& self, * mut u8, Layout) ;
                fn alloc_zeroed(& self, Layout) -> * mut u8;
                fn realloc(& self, * mut u8, Layout, usize) -> * mut u8;
            }
            trait Alloc {
                fn alloc(& mut self, Layout) -> Result < NonNull < u8 > , AllocErr >;
                fn usable_size(& self, & Layout) -> ( usize , usize );
                fn alloc_zeroed(& mut self, Layout) -> Result < NonNull < u8 > , AllocErr >;
                fn alloc_excess(& mut self, Layout) -> Result < Excess , AllocErr >;
                fn alloc_one(& mut self) -> Result < NonNull < T > , AllocErr >;
                fn alloc_array(& mut self, usize) -> Result < NonNull < T > , AllocErr >;
            }
        }

    }


}
