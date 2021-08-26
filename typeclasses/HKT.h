#pragma once
#include "../TypeTraits.h"

template <template <typename...> class URI, typename...>
struct HKT {};

// `* -> *` constructors
template <template <typename...> class _URI, typename _A>
struct HKT<_URI, _A>
{
    template <typename... Args>
    using URI = _URI<Args...>;

    using A = _A;

    using tup = CI::Traits::TypeList<_A>;
};

// `* -> * -> *` constructors
template <template <typename...> class _URI, typename _E, typename _A>
struct HKT<_URI, _E, _A> : HKT<_URI, _A> { using E = _E; };

// `* -> * -> * -> *` constructors
template <template <typename...> class _URI, typename _R, typename _E, typename _A>
struct HKT<_URI, _R, _E, _A> : HKT<_URI, _E, _A> { using R = _R; };

// `* -> * -> * -> * -> *` constructors
template <template <typename...> class _URI, typename _S, typename _R, typename _E, typename _A>
struct HKT<_URI, _S, _R, _E, _A> : HKT<_URI, _R, _E, _A> { using S = _S; };

// Specialize to instruct how to create a type from a template (URI) and HKT arguments
template <typename T>
struct HKT2Kind {};

template <typename HKT>
using Kind_t = typename HKT2Kind<HKT>::type;

// Specialize to instruct how to convert from a kind T to the HKT
template <typename T>
struct Kind2HKT {};

template <typename Kind>
using HKT_t = typename Kind2HKT<Kind>::type;

template <typename T>
using ValueOf_t = typename HKT_t<T>::A;

template <typename T>
using QualifiedValueOf_t = CI::Traits::AddQualifiersFrom_t<T, ValueOf_t<T>>;

template <template <typename...> class URI, typename T>
struct HKTfromTup {};

template <template <typename...> class URI, template <typename...> class L, typename... As>
struct HKTfromTup<URI, L<As...>> { using type = HKT<URI, As...>; };

template <template <typename...> class URI, typename T>
using HKTfromTup_t = typename HKTfromTup<URI, T>::type;

static_assert(std::is_same_v<HKT<CI::Traits::TypeList, int>, HKTfromTup_t<CI::Traits::TypeList, CI::Traits::TypeList<int>>>);

template <typename T, typename U>
struct SetValueOfHKT {};
template <template <typename...> class URI, typename U, typename... Ts>
struct SetValueOfHKT<HKT<URI, Ts...>, U> {
    using list = CI::Traits::TypeList<Ts...>;
    using front = typename CI::Traits::Last<list>::list;
    using newlist = CI::Traits::Concat_t<front, CI::Traits::TypeList<U>>;
    using type = HKTfromTup_t<URI, newlist>;
};

template <typename T, typename U>
using SetValueOfHKT_t = typename SetValueOfHKT<T, U>::type;

static_assert(std::is_same_v<HKT<CI::Traits::TypeList, bool>, SetValueOfHKT_t<HKT<CI::Traits::TypeList, int>, bool>>);

template <typename T, typename U>
using SetValueOf_t = Kind_t<SetValueOfHKT_t<HKT_t<T>, U>>;

template <typename T>
struct SomeType : std::true_type {
    using type = T;
};
struct NoneType : std::false_type {};

template <template <typename...> class Tag, typename Inst>
struct FindClass1
{
    // use the detector pattern to check if we can actually apply
    template <typename C>
    using IsInstance = typename C::is_applied_t;

    using type = std::conditional_t<
        CI::Traits::DetectedOr_t<std::false_type, IsInstance, Tag<Inst>>{},
        SomeType<Tag<Inst>>,
        NoneType
    >;
};

template <template <typename...> class Tag, typename Inst>
using FindClass1_t = typename FindClass1<Tag, Inst>::type;

template <template <template <typename...> class> class Tag, typename Inst>
struct FindClass
{
    using type = NoneType;
};

template <template <template <typename...> class> class Tag, template <typename...> class F, typename... As>
struct FindClass<Tag, F<As...>>
{
    // use the detector pattern to check if we can actually apply
    template <typename C>
    using IsInstance = std::is_same<typename C::is_applied_t, std::true_type>;

    using type = std::conditional_t<
        CI::Traits::DetectedOr_t<std::false_type, IsInstance, Tag<F>>{},
        SomeType<Tag<F>>,
        NoneType
    >;
};

template <template <template <typename...> class> class Tag, typename Inst>
using FindClass_t = typename FindClass<Tag, Inst>::type;

// template <template <typename...> class F>
// struct HKTUtilsBase {
//     template <typename Inst>
//     using IsKind = CI::Traits::IsSpecialization<CI::Traits::RemoveCVRef_t<Inst>, F>;

//     template <typename Inst, CI::Traits::Requires<IsKind<Inst>> = 0>
//     using HeldTypes_t = ;
// };

// template <template <template <typename...> class> class F>
// struct HKTUtils : HKTUtilsBase<F> {};
