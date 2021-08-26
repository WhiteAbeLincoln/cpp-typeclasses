#pragma once
#include "HKT.h"

namespace FunctorUtils
{
    template <template <typename...> class F, typename FInst>
    using IsKind = CI::Traits::IsSpecialization<FInst, F>;

    template <typename FInst, typename Fn>
    using IsMapperFn = std::is_invocable<Fn, QualifiedValueOf_t<FInst>>;

    template <template <typename...> class F, typename FInst, typename Fn>
    using FMapReqs = std::conjunction<IsKind<F, FInst>, IsMapperFn<FInst, Fn>>;

    template <typename FInst>
    using A = QualifiedValueOf_t<FInst>;

    template <typename FInst, typename Fn>
    using B = std::invoke_result_t<Fn, A<FInst>>;

    template <typename FInst, typename Fn>
    using Fb = SetValueOf_t<FInst, B<FInst, Fn>>;
};

template <template <typename...> class F>
struct Functor
{
    template <typename FInst, typename Fn, CI::Traits::Requires<FunctorUtils::FMapReqs<F, FInst, Fn>> = 0>
    static FunctorUtils::Fb<FInst, Fn> fmap(FInst&& instance, Fn&& fn);
};

template <typename FInst, typename Fn, CI::Traits::Requires<FunctorUtils::IsMapperFn<FInst, Fn>> = 0>
constexpr auto fmap(FInst&& inst, Fn&& fn)
{
    using Functor = FindClass_t<Functor, CI::Traits::RemoveCVRef_t<FInst>>;

    if constexpr (Functor{}) {
        using F = typename Functor::type;
        return F::fmap(std::forward<FInst>(inst), std::forward<Fn>(fn));
    }
    //else if constexpr (Monad{}) {
    //    // we can derive fmap if we have a monad instance
    //    using M = typename Monad::type;
    //    return M::mbind(inst, [cap = std::tuple<Fn>(std::forward<Fn>(fn))](auto&& a) {
    //        return M::mreturn(std::invoke(std::get<0>(std::move(cap)), std::move(a)));
    //    });
    //}
    else {
        static_assert(CI::Traits::AlwaysFalse_v<Functor>, "No instance found");
    }
}

