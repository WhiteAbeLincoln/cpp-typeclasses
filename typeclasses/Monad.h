#pragma once
#include "HKT.h"

namespace MonadUtils
{
    template <template <typename...> class M, typename MInst>
    using IsKind = CI::Traits::IsSpecialization<MInst, M>;

    template <typename MInst>
    using A = QualifiedValueOf_t<MInst>;

    template <typename MInst, typename Fn>
    using Mb = std::invoke_result_t<Fn, A<MInst>>;

    template <template <typename...> class M, typename MInst, typename Fn>
    using IsBindFn = CI::Traits::IsSpecialization<M, CI::Traits::Detected_t<Mb, MInst, Fn>>;

    template <template <typename...> class M, typename MInst, typename Fn>
    using MBindReqs = std::conjunction<IsKind<M, MInst>, IsBindFn<M, MInst, Fn>>;
}

template <template <typename...> class M>
struct Monad
{
    template <typename A>
    static Kind_t<M, A> mreturn(A&& val);

    template <typename MInst, typename Fn, CI::Traits::Requires<MonadUtils::MBindReqs<M, MInst, Fn>> = 0>
    static MonadUtils::Mb<MInst, Fn> mbind(MInst&& instance, Fn&& fn);
};

template <template <typename> class M, typename A, typename Class = FindClass_t<Monad, Kind_t<M, A>>, CI::Traits::Requires<Class> = 0>
constexpr Kind_t<M, A> mreturn(A&& val)
{
    return Class::type::mreturn(std::forward<A>(val));
}

template <template <typename, typename, typename...> class M, typename A, typename... Rest>
constexpr CI::Traits::RequiresR<Kind_t<M, Rest..., A>, FindClass_t<Monad, Kind_t<M, Rest..., A>>> mreturn(A&& val)
{
    using Class = FindClass_t<Monad, Kind_t<M, Rest..., A>>;
    return Class::type::mreturn(std::forward<A>(val));
}

template <typename MInst, typename Fn, typename Class = FindClass_t<Monad, CI::Traits::RemoveCVRef_t<MInst>>, CI::Traits::Requires<Class> = 0>
constexpr auto mbind(MInst&& inst, Fn&& fn)
{
    using M = typename Class::type;
    return M::mbind(std::forward<MInst>(inst), std::forward<Fn>(fn));
}


// we can implement fmap in terms of mbind, but we only want to do this if we can't find a functor instance
//template <typename MInst, typename Fn, typename MClass = FindClass_t<Monad, CI::Traits::RemoveCVRef_t<MInst>>>

