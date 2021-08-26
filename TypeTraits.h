#pragma once
#include <type_traits>
#include <tuple>

namespace CI::Traits
{
// backport of c++20 feature
// can be used to establish non-deduced context in template argument deduction
template<typename T>
struct TypeIdentity { using type = T; };
template <typename T>
using TypeIdentity_t = typename TypeIdentity<T>::type;

// tests for TypeIdentity
static_assert(std::is_same_v<size_t, TypeIdentity_t<size_t>>);
static_assert(!std::is_same_v<bool, TypeIdentity_t<size_t>>);

/**
 * Tests whether a type @p T is a specialization of the template @p Tmpl meaning
 * @p T is the template @p Tmpl fully applied.
 *
 * @param T the type to test
 * @param Tmpl the matching template
 */
template <typename T, template <typename...> class Tmpl>
struct IsSpecialization : std::false_type {};
template <template <typename...> class Tmpl, typename... Args>
struct IsSpecialization<Tmpl<Args...>, Tmpl>: std::true_type {};
template <typename T, template <typename...> class Tmpl>
inline constexpr bool IsSpecialization_v = IsSpecialization<T, Tmpl>::value;

// tests for IsSpecialization
static_assert(IsSpecialization_v<std::tuple<size_t>, std::tuple>);
static_assert(!IsSpecialization_v<size_t, std::tuple>);

namespace detail
{
    // from https://en.cppreference.com/w/cpp/experimental/is_detected
    struct nonesuch {
        nonesuch() = delete;
        ~nonesuch() = delete;
        nonesuch(nonesuch const&) = delete;
        void operator=(nonesuch const&) = delete;
    };
    template <typename Default, typename AlwaysVoid, template <typename...> class Op, typename... Args>
    struct detector {
        using value_t = std::false_type;
        using type = Default;
    };

    template <typename Default, template <typename...> class Op, typename... Args>
    struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
        using value_t = std::true_type;
        using type = Op<Args...>;
    };
}

/**
 * Tests whether the template @p Op applied to the arguments @p Args denotes a valid type
 * Commonly used to check for the existence of properties on a struct.
 * @param Op The template operation to test
 * @param Args The argments which @p Op will be applied to
 */
template <template <typename...> class Op, typename... Args>
using IsDetected = typename detail::detector<detail::nonesuch, void, Op, Args...>::value_t;
template <template <typename...> class Op, typename... Args>
inline constexpr bool IsDetected_v = IsDetected<Op, Args...>::value;

// tests for Detected
static_assert(IsDetected_v<std::tuple, size_t>);
static_assert(!IsDetected_v<std::is_same, size_t>);

/**
 * Returns Op<Args...> if the construction is valid, otherwise the type nonesuch
 */
template <template <typename...> class Op, typename... Args>
using Detected_t = typename detail::detector<detail::nonesuch, void, Op, Args...>::type;

// tests for DetectedT
static_assert(std::is_same_v<std::tuple<size_t>, Detected_t<std::tuple, size_t>>);
static_assert(std::is_same_v<detail::nonesuch, Detected_t<std::is_same, size_t>>);

/**
 * Returns Op<Args...> if the construction is valid, otherwise the given Default type
 */
template <typename Default, template <typename...> class Op, typename... Args>
using DetectedOr = detail::detector<Default, void, Op, Args...>;
template <typename Default, template <typename...> class Op, typename... Args>
using DetectedOr_t = typename DetectedOr<Default, Op, Args...>::type;

// tests for DetectedOr
static_assert(std::is_same_v<std::tuple<size_t>, DetectedOr_t<std::false_type, std::tuple, size_t>>);
static_assert(std::is_same_v<std::false_type, DetectedOr_t<std::false_type, std::is_same, size_t>>);

/**
 * Tests whether Op<Args...> was exactly equal to the Expected type
 */
template <typename Expected, template <typename...> class Op, typename... Args>
using IsDetectedExact = std::is_same<Expected, Detected_t<Op, Args...>>;
template <typename Expected, template <typename...> class Op, typename... Args>
inline constexpr bool IsDetectedExact_v = IsDetectedExact<Expected, Op, Args...>::value;

// tests for IsDetectedExact
static_assert(IsDetectedExact_v<size_t, TypeIdentity_t, size_t>);
static_assert(!IsDetectedExact_v<int, TypeIdentity_t, size_t>);

/**
 * Tests whether Op<Args...> was convertible to the Expected type
 */
template <typename Expected, template <typename...> class Op, typename... Args>
using IsDetectedConvertible = std::is_convertible<Detected_t<Op, Args...>, Expected>;
template <typename Expected, template <typename...> class Op, typename... Args>
inline constexpr bool IsDetectedConvertible_v = IsDetectedConvertible<Expected, Op, Args...>::value;

// tests for IsDetectedConvertible
static_assert(IsDetectedConvertible_v<size_t, TypeIdentity_t, size_t>);
static_assert(IsDetectedConvertible_v<int, TypeIdentity_t, size_t>);
static_assert(!IsDetectedConvertible_v<std::tuple<size_t, size_t>, TypeIdentity_t, size_t>);

namespace detail
{
    template <typename T>
    using HasStrData = decltype(std::declval<T const>().data());
    template <typename T>
    using HasStrSize = decltype(std::declval<T>().size());
    // for the test, never initialized or called so no overhead
    struct IsStrTest1 { const char* data() const; size_t size(); };
    struct IsStrTest2 { uint8_t* data() const; size_t size(); };
    struct IsStrTest3 { const char* data() const; double size(); };
    struct IsStrTest4 { const char* data() const; };
    struct IsStrTest5 { };
}

/**
 * Tests whether @p T matches some common string types (std::string, CStdString, std::string_view)
 * @param T The type to test
 */
template <typename T>
using IsStdStringLike = std::conjunction<IsDetectedExact<const char*, detail::HasStrData, std::decay_t<T>>, std::is_integral<Detected_t<detail::HasStrSize, std::decay_t<T>>>>;
template <typename T>
inline constexpr bool IsStdStringLike_v = IsStdStringLike<T>::value;


// tests for IsStdStringLike
static_assert(IsStdStringLike_v<detail::IsStrTest1>);
static_assert(!IsStdStringLike_v<detail::IsStrTest2>);
static_assert(!IsStdStringLike_v<detail::IsStrTest3>);
static_assert(!IsStdStringLike_v<detail::IsStrTest4>);
static_assert(!IsStdStringLike_v<detail::IsStrTest5>);

/**
 * Tests whether @p T is a c-style string (const char*)
 * @param T The type to test
 */
template <typename T>
using IsCString = std::is_same<std::decay_t<T>, const char*>;
template <typename T>
inline constexpr bool IsCString_v = IsCString<T>::value;

// tests for IsCString
static_assert(IsCString_v<const char*>);
static_assert(IsCString_v<const char[2]>);
static_assert(!IsCString_v<size_t>);

/**
 * use to ensure templates meet a set of requirements
 * @param Assertions Templates which are all std::true_type or std::false_type
 */
template <typename... Assertions>
using Requires = std::enable_if_t<std::conjunction_v<Assertions...>, int>;

/**
 * use to ensure templates meet one of a set of requirements
 * @param Assertions Templates which are all std::true_type or std::false_type
 */
template <typename... Assertions>
using RequiresAny = std::enable_if_t<std::disjunction_v<Assertions...>, int>;

/**
 * Use to ensure templates meet a set of requirements. Use in place of the return
 * type of a function if you have a variadic template and cannot use Requires.
 * @param R the return type
 * @param Assertions Templates which are all std::true_type or std::false_type
 */
template <typename R, typename... Assertions>
using RequiresR = std::enable_if_t<std::conjunction_v<Assertions...>, R>;

/**
 * Used to test whether a type @p T passes some of the given type predicates @p Preds
 * @param T the type to test
 * @param Preds the predicates to test against
 */
template <typename T, template <typename> class... Preds>
using SomeOf = std::disjunction<Preds<T>...>;
template <typename T, template <typename> class... Preds>
inline constexpr bool SomeOf_v = SomeOf<T, Preds...>::value;

// tests for SomeOf
static_assert(SomeOf_v<const char*, IsCString, IsStdStringLike>);
static_assert(!SomeOf_v<const char*, IsStdStringLike>);

/**
 * Used to test whether a type @p T passes all of the given type predicates @p Preds
 * @param T the type to test
 * @param Preds the predicates to test against
 */
template <typename T, template <typename> class... Preds>
using AllOf = std::conjunction<Preds<T>...>;
template <typename T, template <typename> class... Preds>
inline constexpr bool AllOf_v = AllOf<T, Preds...>::value;

// tests for AllOf
static_assert(AllOf_v<size_t, std::is_integral, std::is_arithmetic>);
static_assert(!AllOf_v<size_t, std::is_integral, std::is_function>);

namespace detail
{
// adapted from Boost MP11 mp_rename
template <typename A, template <typename...> class B>
struct RenameImpl;
template <template <typename...> class A, typename... T, template <typename...> class B>
struct RenameImpl<A<T...>, B> { using type = B<T...>; };
}

/**
 * Used to rename a type's template, i.e. to convert from a template A<Args...> to B<Args...>
 * @param A a type which is an application of some template
 * @param B a new template
 */
template <typename A, template <typename...> class B>
using Rename = typename detail::RenameImpl<A, B>::type;

// tests for Rename
static_assert(std::is_same_v<TypeIdentity<size_t>, Rename<std::tuple<size_t>, TypeIdentity>>);
static_assert(!std::is_same_v<std::tuple<size_t>, Rename<std::tuple<size_t>, TypeIdentity>>);

/**
 * Tests whether a set of types @p Ts has a common type (as defined by std::common_type).
 * @param Ts the types to test
 */
template <typename... Ts>
using HasCommonType = IsDetected<std::common_type_t, Ts...>;
template <typename... Ts>
inline constexpr bool HasCommonType_v = HasCommonType<Ts...>::value;

// tests for HasCommonType
static_assert(HasCommonType_v<size_t, bool, char>);
static_assert(!HasCommonType_v<size_t, const char[2]>);

namespace detail
{
// adapted from https://stackoverflow.com/a/57528226
template <typename T, typename... Ts>
struct UniqueImpl { using type = T; };

template <template <typename...> typename L, typename... Ts, typename U, typename... Us>
struct UniqueImpl<L<Ts...>, U, Us...>
: std::conditional_t<(std::is_same_v<U, Ts> || ...)
  , UniqueImpl<L<Ts...>, Us...>
  , UniqueImpl<L<Ts..., U>, Us...>> {};

template <typename T>
struct MkUniqueImpl;

template <template <typename...> typename L, typename... Ts>
struct MkUniqueImpl<L<Ts...>> : UniqueImpl<L<>, Ts...> {};
}

/**
 * Filters duplicates from the list of types @p Ts returning a
 * tuple of the unique types. This tuple can then be changed to another
 * type using Rename.
 *
 * @param Ts the types to filter
 *
 * @example
 * using Tup = UniqueTuple<std::string, size_t, size_t>;
 * // Tup = std::tuple<std::string, size_t>
 * using UniqueVariant = Rename<Tup, std::variant>;
 * // UniqueVariant = std::variant<std::string, size_t>
 */
template <typename... Ts>
using UniqueTuple = typename detail::UniqueImpl<std::tuple<>, Ts...>::type;

//  tests for UniqueTuple
static_assert(std::is_same_v<std::tuple<char, bool, size_t>, UniqueTuple<char, bool, size_t, bool, char>>);
static_assert(!std::is_same_v<std::tuple<char, bool, size_t, bool, char>, UniqueTuple<char, bool, size_t, bool, char>>);

/**
 * Filters duplicates from the given list-like type @p T.
 *
 * @param T the list-like type to filter
 *
 * @example
 * using Var = MkUnique<std::variant<std::string, size_t, size_t>>;
 * // Var = std::variant<std::string, size_t>
 */
template <typename T>
using Unique = typename detail::MkUniqueImpl<T>::type;

//  tests for MkUnique
static_assert(std::is_same_v<std::tuple<char, bool, size_t>, Unique<std::tuple<char, bool, size_t, bool, char>>>);
static_assert(!std::is_same_v<std::tuple<char, bool, size_t, bool, char>, Unique<std::tuple<char, bool, size_t, bool, char>>>);

// gets the template argument at the given position
template <typename T, size_t Idx>
using GetTemplateArg = std::tuple_element_t<Idx, Rename<T, std::tuple>>;

// tests for GetTemplateArg
static_assert(std::is_same_v<const char*, GetTemplateArg<IsCString<const char*>, 0>>);

// helper value for exhaustive visitors
template<class>
inline constexpr bool AlwaysFalse_v = false;

template <typename... As>
struct TypeList
{
    using size = std::integral_constant<size_t, sizeof... (As)>;
};

template <typename... L>
struct Concat;

template<> struct Concat<>
{
    using type = TypeList<>;
};

template <template <typename...> class L, typename... T>
struct Concat<L<T...>>
{
    using type = L<T...>;
};

template <
    template <typename...> class L1, typename... Xs,
    template <typename...> class L2, typename... Ys,
    typename... Rest
>
struct Concat<L1<Xs...>, L2<Ys...>, Rest...> : Concat<L1<Xs..., Ys...>, Rest...> {};

template <typename... L>
using Concat_t = typename Concat<L...>::type;

// tests for concat
static_assert(std::is_same_v<TypeList<>, Concat_t<>>);
static_assert(std::is_same_v<std::tuple<int, bool>, Concat_t<std::tuple<int, bool>>>);
static_assert(std::is_same_v<std::tuple<int, bool, char>, Concat_t<std::tuple<int, bool>, TypeIdentity<char>>>);
static_assert(std::is_same_v<std::tuple<int, bool, char, double>, Concat_t<std::tuple<int, bool>, TypeIdentity<char>, TypeList<double>>>);

namespace detail
{
    template <typename TAcc, typename... Ts>
    struct FlattenImpl { using type = TAcc; };

    template <template<typename...> typename L, typename... AccTs, typename... TupTs, typename... Rest>
    struct FlattenImpl<L<AccTs...>, L<TupTs...>, Rest...>
    : FlattenImpl<L<AccTs..., TupTs...>, Rest...> {};

    template <template<typename...> typename L, typename... AccTs, typename T, typename... Rest>
    struct FlattenImpl<L<AccTs...>, T, Rest...>
    : FlattenImpl<L<AccTs..., T>, Rest...> {};

    template <template<typename...> typename L, typename... AccTs>
    struct FlattenImpl<L<AccTs...>, L<>>
    : FlattenImpl<L<AccTs...>> {};

    template <typename T>
    struct FlattenArgImpl;

    template <template <typename...> typename L, typename... Ts>
    struct FlattenArgImpl<L<Ts...>> : FlattenImpl<L<>, Ts...> {};
}

/**
* Flattens the given list-like type
* @param T the list type
*/
template <typename T>
using Flatten = typename detail::FlattenArgImpl<T>::type;

// tests for FlattenTuple
static_assert(std::is_same_v<std::tuple<int, bool, int, bool>, Flatten<std::tuple<int, bool, std::tuple<int, bool>>>>);
static_assert(!std::is_same_v<std::tuple<int, bool, int, bool>, Flatten<std::tuple<int, bool, std::tuple<bool>>>>);
static_assert(!std::is_same_v<std::tuple<int, bool, bool>, Flatten<std::tuple<int, bool, TypeIdentity<bool>>>>);
static_assert(std::is_same_v<std::tuple<int, bool, std::tuple<bool>, TypeIdentity<bool>>, Flatten<std::tuple<int, std::tuple<bool, std::tuple<bool>>, TypeIdentity<bool>>>>);

// adapted from https://stackoverflow.com/a/42583794

/**
 * Checks if a type T is contained in the list Us
 */
template <typename T, typename... Us>
struct Contains : std::disjunction<std::is_same<T, Us>...> {};
template <typename T, typename... Us>
inline constexpr bool Contains_v = Contains<T, Us...>::value;

template <typename...>
struct ListContains : std::false_type {};
template <template <typename...> typename L, typename T, typename... Us>
struct ListContains<T, L<Us...>> : Contains<T, Us...> {};
/**
 * Checks if the type T is in the type list for the list like type L
 * (where list like is something like std::tuple, std::variant)
 */
template <typename T, typename L>
inline constexpr bool ListContains_v = ListContains<T, L>::value;

template <typename...>
struct IsSubsetOf : std::false_type {};

/**
 * Checks if a type list Ts is a subset of the type list Us
 */
template <template <typename...> typename L, typename... Ts, typename... Us>
struct IsSubsetOf<L<Ts...>, L<Us...>> : std::conjunction<Contains<Ts, Us...>...> {};
/**
 * Checks if a type list Ts is a subset of the type list Us
 * @param Ts a type list (a template containing a variadic list of types)
 * @param Us a type list
 * @returns if the types in Ts are a subset (not strict) of the types in Us
 */
template <typename... Args>
inline constexpr bool IsSubsetOf_v = IsSubsetOf<Args...>::value;

// tests for Contains and IsSubsetOf
static_assert(!IsSubsetOf_v<std::tuple<double, int, char, bool>, std::tuple<int, bool, char>>);
static_assert(IsSubsetOf_v<std::tuple<int, char, bool>, std::tuple<int, bool, char>>);
static_assert(IsSubsetOf_v<std::tuple<int, bool>, std::tuple<int, bool, char>>);
static_assert(IsSubsetOf_v<std::tuple<int>, std::tuple<int, bool, char>>);
static_assert(IsSubsetOf_v<std::tuple<>, std::tuple<int, bool, char>>);
// fails because the lists don't use the same list template type (TypeIdentity vs std::tuple)
static_assert(!IsSubsetOf_v<TypeIdentity<int>, std::tuple<int, bool, char>>);
static_assert(Contains_v<int, int, bool, char>);
static_assert(Contains_v<bool, int, bool, char>);
static_assert(Contains_v<char, int, bool, char>);
static_assert(!Contains_v<double, int, bool, char>);
static_assert(ListContains_v<int, std::tuple<int, bool, char>>);
static_assert(ListContains_v<bool, std::tuple<int, bool, char>>);
static_assert(ListContains_v<char, std::tuple<int, bool, char>>);
static_assert(!ListContains_v<double, std::tuple<int, bool, char>>);

template <template <typename...> class C, typename Inst, size_t N>
struct NthTypeParam {};

template <template <typename...> class C, size_t N, typename... Args>
struct NthTypeParam<C, C<Args...>, N>
{
    using type = std::tuple_element_t<N, std::tuple<Args...>>;
};

template <template <typename...> class C, typename Inst, size_t N>
using NthTypeParam_t = typename NthTypeParam<C, std::remove_reference_t<Inst>, N>::type;

static_assert(std::is_same_v<int, NthTypeParam_t<TypeIdentity, TypeIdentity<int>, 0>>);
static_assert(std::is_same_v<int, NthTypeParam_t<TypeIdentity, TypeIdentity<int>&, 0>>);
static_assert(std::is_same_v<int, NthTypeParam_t<TypeIdentity, TypeIdentity<int>&&, 0>>);

template <class T, class U>
struct AddQualifiersFrom { using type = U; };
template <class T, class U>
struct AddQualifiersFrom<const T, U> { using type = const typename AddQualifiersFrom<std::remove_const_t<T>, U>::type; };
template <class T, class U>
struct AddQualifiersFrom<volatile T, U> { using type = volatile typename AddQualifiersFrom<std::remove_volatile_t<T>, U>::type; };
template <class T, class U>
struct AddQualifiersFrom<const volatile T, U> { using type = const volatile typename AddQualifiersFrom<std::remove_cv_t<T>, U>::type; };
template <class T, class U>
struct AddQualifiersFrom<T&&, U> { using type = typename AddQualifiersFrom<std::remove_reference_t<T>, U>::type&&; };
template <class T, class U>
struct AddQualifiersFrom<T&, U> { using type = typename AddQualifiersFrom<std::remove_reference_t<T>, U>::type&; };

template <class T, class U>
using AddQualifiersFrom_t = typename AddQualifiersFrom<T, U>::type;

static_assert(std::is_same_v<int, AddQualifiersFrom_t<bool, int>>);
static_assert(std::is_same_v<const int, AddQualifiersFrom_t<const bool, int>>);
static_assert(std::is_same_v<const volatile int, AddQualifiersFrom_t<const volatile bool, int>>);
static_assert(std::is_same_v<int&, AddQualifiersFrom_t<bool&, int>>);
static_assert(std::is_same_v<const int&, AddQualifiersFrom_t<const bool&, int>>);
static_assert(std::is_same_v<int&&, AddQualifiersFrom_t<bool&&, int>>);
static_assert(std::is_same_v<const int&&, AddQualifiersFrom_t<const bool&&, int>>);

template <typename T>
struct RemoveCVRef { using type = std::remove_cv_t<std::remove_reference_t<T>>; };
template <typename T>
using RemoveCVRef_t = typename RemoveCVRef<T>::type;

template <typename Inst>
struct GetTemplateImpl {};
template <template <typename...> class C, typename... Ts>
struct GetTemplateImpl<C<Ts...>>
{
    using Arity = std::integral_constant<size_t, sizeof...(Ts)>;

    template <typename... As>
    using Tmpl = C<As...>;
};

template <typename Inst>
using GetTemplate = GetTemplateImpl<RemoveCVRef_t<Inst>>;

static_assert(1 == GetTemplate<TypeIdentity<int>>::Arity{});
static_assert(std::is_same_v<GetTemplate<TypeIdentity<int>>::template Tmpl<int>, TypeIdentity<int>>);

/*
partitionAt n acc (x:xs) = if n == (len acc) then (acc, xs) else partitionAt n (acc ++ [x]) xs
partitionAt n acc [] = if (len acc) == n then (acc, []) else ()
*/
template <size_t N, typename X, typename Y>
struct PartitionAtImpl {};

/* partitionAt n acc [] = if (len acc) < n then () else (acc, []) */
template <template <typename...> class L, size_t N, typename... Xs>
struct PartitionAtImpl<N, L<Xs...>, L<>>
{
    using type = std::conditional_t<(sizeof... (Xs) == N), L<L<Xs...>, L<>>, L<>>;
};

/* partitionAt n acc (x:xs) = if n == (len acc) then (acc, (x:xs)) else partitionAt n (acc ++ [x]) xs */
template <template <typename...> class L, size_t N, typename... Ys, typename X, typename... Xs>
struct PartitionAtImpl<N, L<Ys...>, L<X, Xs...>>
{
    using type = std::conditional_t<(N == sizeof... (Ys)), L<L<Ys...>, L<X, Xs...>>, typename PartitionAtImpl<N, L<Ys..., X>, L<Xs...>>::type>;
};

template <size_t N, typename T>
struct PartitionAt {};
template <template <typename...> class L, typename... Xs, size_t N>
struct PartitionAt<N, L<Xs...>> : PartitionAtImpl<N, L<>, L<Xs...>> {};

template <size_t N, typename T>
using PartitionAt_t = typename PartitionAt<N, T>::type;

static_assert(std::is_same_v<TypeList<TypeList<>, TypeList<int, bool, char>>, PartitionAt_t<0, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<TypeList<TypeList<int>, TypeList<bool, char>>, PartitionAt_t<1, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<TypeList<TypeList<int, bool>, TypeList<char>>, PartitionAt_t<2, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<TypeList<TypeList<int, bool, char>, TypeList<>>, PartitionAt_t<3, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<std::tuple<std::tuple<int, bool, char>, std::tuple<>>, PartitionAt_t<3, std::tuple<int, bool, char>>>);
static_assert(std::is_same_v<TypeList<>, PartitionAt_t<4, TypeList<int, bool, char>>>);

template <size_t N, typename T>
struct ElementAt{};
template <template <typename...> class L, typename... Xs, size_t N>
struct ElementAt<N, L<Xs...>> : std::tuple_element<N, std::tuple<Xs...>> {};

template <size_t N, typename T>
using ElementAt_t = typename ElementAt<N, T>::type;

static_assert(std::is_same_v<int, ElementAt_t<0, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<bool, ElementAt_t<1, TypeList<int, bool, char>>>);
static_assert(std::is_same_v<char, ElementAt_t<2, TypeList<int, bool, char>>>);

template <typename T>
struct ListSize {};
template <template <typename...> class L, typename... Xs>
struct ListSize<L<Xs...>> : std::integral_constant<size_t, sizeof... (Xs)> {};

template <typename T>
inline constexpr size_t ListSize_v = ListSize<T>::value;

static_assert(3u == ListSize_v<TypeList<int, bool, char>>);
static_assert(3u == ListSize_v<std::tuple<int, bool, char>>);

template <typename T>
struct Head {};

template <template <typename...> class L, typename X, typename... Xs>
struct Head<L<X, Xs...>>
{
    using type = X;
    using list = L<Xs...>;
};

static_assert(std::is_same_v<int, typename Head<TypeList<int, bool, char>>::type>);
static_assert(std::is_same_v<TypeList<bool, char>, typename Head<TypeList<int, bool, char>>::list>);

template <typename T>
struct Last
{
private:
    using partitioned = PartitionAt_t<ListSize_v<T> - 1, T>;
public:
    using type = ElementAt_t<0, ElementAt_t<1, partitioned>>;
    using list = ElementAt_t<0, partitioned>;
};

static_assert(std::is_same_v<int, typename Last<TypeList<int>>::type>);
static_assert(std::is_same_v<TypeList<>, typename Last<TypeList<int>>::list>);
static_assert(std::is_same_v<char, typename Last<TypeList<int, bool, char>>::type>);
static_assert(std::is_same_v<TypeList<int, bool>, typename Last<TypeList<int, bool, char>>::list>);

/**
 * Adds the reference type of U (rvalue, lvalue, or none) to T
 */
template <typename T, typename U>
using AddReferenceFrom_t = std::conditional_t<
    std::is_lvalue_reference_v<U>,
    std::add_lvalue_reference_t<T>,
    std::conditional_t<
        std::is_rvalue_reference_v<U>,
        std::add_rvalue_reference_t<T>,
        T
    >
> ;

template <typename U, typename T>
constexpr AddReferenceFrom_t<std::remove_reference_t<T>, U> moveas(T&& arg) noexcept
{
    return static_cast<AddReferenceFrom_t<std::remove_reference_t<T>, U>>(arg);
}

namespace detail
{
    struct passepartout
    {
        template <typename T>
        operator T& ();
        template <typename T>
        operator T&& ();
    };
}

using Arg = detail::passepartout&;
static_assert(std::is_invocable_v<void (int&&), Arg>);
static_assert(std::is_invocable_v<void (int&), Arg>);
static_assert(std::is_invocable_v<void (int), Arg>);
static_assert(std::is_invocable_v<void (int, int), Arg, Arg>);
static_assert(!std::is_invocable_v<void (int, int), Arg>);
static_assert(!std::is_invocable_v<void (int, int)>);

}
