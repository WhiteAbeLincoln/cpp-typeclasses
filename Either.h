#pragma once
#include "HKT.h"
#include "TypeTraits.h"
#include "Functor.h"
#include <variant>
#include <utility>

template <typename T> class CRight { T value; };
template <typename T> class CLeft { T value; };

template <typename R>
constexpr CRight<R> Right(R&& val) { return { std::forward<R>(val) }; }
template <typename L>
constexpr CLeft<L> Left(L&& val) { return { std::forward<L>(val) }; }

template <typename L, typename R>
class CEither : public std::variant<L, R>
{
public:
    using LeftType = L;
    using RightType = R;
public:
    using std::variant<L, R>::variant;
    template<std::size_t I, class... Args, CI::Traits::Requires<std::bool_constant<I < 2>> = 0>
    constexpr explicit CEither(std::in_place_index_t<I> idx, Args&&... args) : std::variant<L, R>(idx, std::forward<Args>(args)...) {}

    template <typename LL, CI::Traits::Requires<std::is_convertible<LL, L>, std::is_copy_constructible<LL>> = 0>
    CEither(const CLeft<LL>& l) : CEither(std::in_place_index_t<0>{}, l.value) {}
    template <typename LL, CI::Traits::Requires<std::is_convertible<LL, L>> = 0>
    CEither(CLeft<LL>&& l) : CEither(std::in_place_index_t<0>{}, std::move(l.value)) {}

    template <typename RR, CI::Traits::Requires<std::is_convertible<RR, R>, std::is_copy_constructible<RR>> = 0>
    CEither(const CRight<RR>& r) : CEither(std::in_place_index_t<1>{}, r.value) {}
    template <typename RR, CI::Traits::Requires<std::is_convertible<RR, R>> = 0>
    CEither(CRight<RR>&& r) : CEither(std::in_place_index_t<1>{}, std::move(r.value)) {}

    template <typename LL>
    static constexpr auto Left(LL&& val) { return CEither<LL, R>(::Left(std::forward<LL>(val))); }
    template <typename RR>
    static constexpr auto Right(RR&& val) { return CEither<L, RR>(::Right(std::forward<RR>(val))); }

    constexpr explicit operator bool() const noexcept { return IsRight(); }
    constexpr bool IsLeft() const noexcept { return this->index() == 0; }
    constexpr bool IsRight() const noexcept { return this->index() == 1; }

    constexpr auto GetLeft() noexcept { return std::get_if<0>(this); }
    constexpr auto GetLeft() const noexcept { return std::get_if<0>(this); }

    constexpr auto GetRight() noexcept { return std::get_if<1>(this); }
    constexpr auto GetRight() const noexcept { return std::get_if<1>(this); }
};

namespace concepts
{
    template <typename T>
    using IsEither = CI::Traits::IsSpecialization<T, CEither>;
    template <typename T>
    inline constexpr bool IsEither_v = IsEither<T>::value;
    static_assert(IsEither_v<CEither<int, bool>>);

    template <typename T>
    using GetLeftVal = decltype(std::get<0>(std::declval<T>()));
    static_assert(std::is_same_v<int&&, GetLeftVal<CEither<int, bool>>>);
    static_assert(std::is_same_v<int&, GetLeftVal<CEither<int, bool>&>>);
    static_assert(std::is_same_v<const int&, GetLeftVal<const CEither<int, bool>&>>);

    template <typename T>
    using GetRightVal = decltype(std::get<1>(std::declval<T>()));
    static_assert(std::is_same_v<bool&&, GetRightVal<CEither<int, bool>>>);
    static_assert(std::is_same_v<bool&, GetRightVal<CEither<int, bool>&>>);
    static_assert(std::is_same_v<const bool&, GetRightVal<const CEither<int, bool>&>>);

    template <typename T>
    using RightType_t = typename std::remove_reference_t<T>::RightType;
    static_assert(std::is_same_v<int, RightType_t<CEither<bool, int>>>);
    static_assert(std::is_same_v<int, RightType_t<CEither<bool, int>&&>>);
    static_assert(std::is_same_v<int, RightType_t<CEither<bool, int>&>>);
    static_assert(std::is_same_v<int, RightType_t<const CEither<bool, int>&>>);
    template <typename T>
    using LeftType_t = typename std::remove_reference_t<T>::LeftType;
    static_assert(std::is_same_v<bool, LeftType_t<CEither<bool, int>>>);
    static_assert(std::is_same_v<bool, LeftType_t<CEither<bool, int>&&>>);
    static_assert(std::is_same_v<bool, LeftType_t<CEither<bool, int>&>>);
    static_assert(std::is_same_v<bool, LeftType_t<const CEither<bool, int>&>>);
    static_assert(std::is_same_v<bool, LeftType_t<CEither<bool, int> const &>>);
}

// -----------------------
// register the instance
// -----------------------
template <typename E, typename A>
struct URItoKind<CEither, E, A> { using type = CEither<E, A>; };
template <typename L, typename R> 
struct KindTypeOrder<CEither, L, R> { using type = CI::Traits::TypeList<L, R>; };

static_assert(std::is_same_v<CEither<bool, int>, Kind_t<CEither, bool, int>>);
static_assert(std::is_same_v<bool, ValueOf_t<CEither<int, bool>>>);
static_assert(std::is_same_v<CEither<int, char>, SetValueOf_t<CEither<int, bool>, char>>);
// -----------------------

template <>
struct Functor<CEither>
{
    using is_applied_t = std::true_type;

    template <typename FInst, typename Fn>
    static FunctorUtils::Fb<FInst, Fn> fmap(FInst&& instance, Fn&& fn)
    {
        using Next = FunctorUtils::Fb<FInst, Fn>;
        if (instance) {
            return Next::Right(std::invoke(std::forward<Fn>(fn), std::get<0>(std::forward<FInst>(instance))));
        }
        else {
            return Next::Left(std::get<1>(std::forward<FInst>(instance)));
        }
    }

};

