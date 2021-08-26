#pragma once
#include <ostream>
#include "Show.h"

template <typename T>
using CallOstream = decltype(std::declval<std::ostream&>() << std::declval<const T&>());

template <typename T>
using HasOstream = CI::Traits::IsDetected<CallOstream, T>;

template <typename T, CI::Traits::Requires<ShowUtils::IsShowable<T>, std::negation<HasOstream<T>>> = 0>
std::ostream& operator<<(std::ostream& os, const T& data)
{
    os << ::show(data);
    return os;
}

