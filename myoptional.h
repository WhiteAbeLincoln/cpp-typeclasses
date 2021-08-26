#pragma once
#include "HKT.h"
#include "Functor.h"
#include "Monad.h"
#include "Show.h"
#include <optional>

// -----------------------
// register the instance
// -----------------------
template <typename A>
struct HKT2Kind<HKT<std::optional, A>> { using type = std::optional<A>; };
template <typename _A>
struct Kind2HKT<std::optional<_A>> { using type = HKT<std::optional, _A>; };

static_assert(std::is_same_v<std::optional<int>, Kind_t<HKT<std::optional, int>>>);
static_assert(std::is_same_v<HKT<std::optional, int>, HKT_t<std::optional<int>>>);
static_assert(std::is_same_v<int, ValueOf_t<std::optional<int>>>);
// -----------------------

template <>
struct Functor<std::optional>
{
    using is_applied_t = std::true_type;

    template <typename FInst, typename Fn>
    static FunctorUtils::Fb<FInst, Fn> fmap(FInst&& instance, Fn&& fn)
    {
        if (instance) {
            return std::optional(std::invoke(std::forward<Fn>(fn), *std::forward<FInst>(instance)));
        }
        else {
            return std::nullopt;
        }
    }
};

template <>
struct Monad<std::optional>
{
    using is_applied_t = std::true_type;

    template <typename A>
    static std::optional<A> mreturn(A&& val)
    {
        return std::optional(std::forward<A>(val));
    }

    template <typename MInst, typename Fn>
    static MonadUtils::Mb<MInst, Fn> mbind(MInst&& instance, Fn&& fn)
    {
        if (instance) {
            return std::invoke(std::forward<Fn>(fn), *std::forward<MInst>(instance));
        }
        else {
            return std::nullopt;
        }
    }
};

template <typename A>
struct Show<std::optional<A>>
{
    using is_applied_t = ShowUtils::IsShowable<A>;

    static CI::Traits::RequiresR<std::string, ShowUtils::IsShowable<A>> show(const std::optional<A>& instance)
    {
        if (instance) {
            return "std::optional{ " + ::show(*instance) + " }";
        }
        return "std::nullopt";
    }
};
