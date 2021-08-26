#pragma once
#include "typeclasses/HKT.h"
#include "typeclasses/Functor.h"
#include "typeclasses/Show.h"
#include <vector>
#include <algorithm>
#include <iterator>

// -----------------------
// register the instance
// -----------------------
// we always use the default allocator - FIX: find a method that allows mapping between allocator types
template <typename A>
struct HKT2Kind<HKT<std::vector, A>> { using type = std::vector<A>; };
template <typename A, typename Alloc>
struct Kind2HKT<std::vector<A, Alloc>> { using type = HKT<std::vector, A>; };

static_assert(std::is_same_v<std::vector<int>, Kind_t<HKT<std::vector, int>>>);
static_assert(std::is_same_v<HKT<std::vector, int>, HKT_t<std::vector<int>>>);
static_assert(std::is_same_v<int, ValueOf_t<std::vector<int>>>);
static_assert(std::is_same_v<std::vector<char>, SetValueOf_t<std::vector<int>, char>>);
// -----------------------

template <>
struct Functor<std::vector>
{
    using is_applied_t = std::true_type;

    template <typename FInst, typename Fn>
    static FunctorUtils::Fb<FInst, Fn> fmap(FInst&& instance, Fn&& fn)
    {
        using Result = FunctorUtils::Fb<FInst, Fn>;
        Result aRet;
        aRet.reserve(instance.size());

        // if they have the same type, we can move or copy the vector
        // then map in place
        if constexpr (std::is_same_v<Result, CI::Traits::RemoveCVRef_t<decltype(instance)>>) {
            aRet = std::forward<FInst>(instance);
            for (size_t i = 0; i < aRet.size(); ++i) {
                aRet[i] = std::invoke(fn, CI::Traits::moveas<decltype(instance)>(aRet[i]));
            }
            return aRet;
        }

        if constexpr (std::is_rvalue_reference_v<decltype(instance)>) {
            std::transform(std::make_move_iterator(instance.begin()), std::make_move_iterator(instance.end()), std::back_inserter(aRet), std::forward<Fn>(fn));
        }
        else {
            std::transform(instance.begin(), instance.end(), std::back_inserter(aRet), std::forward<Fn>(fn));
        }

        return aRet;
    }
};

template <typename A>
struct Show<std::vector<A>>
{
    using is_applied_t = ShowUtils::IsShowable<A>;

    static CI::Traits::RequiresR<std::string, ShowUtils::IsShowable<A>> show(const std::vector<A>& instance)
    {
        std::string s;
        s += "";
        for (const auto& elem : instance) {
            s += ::show(elem) + ", ";
        }
        if (!s.empty()) {
            // remove trailing ", "
            s.pop_back();
            s.pop_back();
        }
        else {
            return "{}";
        }

        return "{ " + s + " }";
    }
};
