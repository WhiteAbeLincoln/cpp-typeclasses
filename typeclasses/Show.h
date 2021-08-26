#pragma once
#include "HKT.h"
#include "../TypeTraits.h"
#include <string>

template <typename T>
using ToString = decltype(std::to_string(std::declval<T>()));

template <typename T>
using IsToStringable = CI::Traits::IsDetected<ToString, T>;

template <typename T>
struct Show
{
    static std::string show(const T& instance);
};

namespace ShowUtils
{
    template <typename T>
    using IsShowable = std::disjunction<FindClass1_t<Show, T>, std::is_same<char, T>, IsToStringable<T>, std::is_same<std::string, T>>;
};

std::string ReplaceAll(std::string str, const std::string& from, const std::string& to) {
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
    }
    return str;
}

template <typename T, CI::Traits::Requires<ShowUtils::IsShowable<T>> = 0>
constexpr std::string show(const T& instance)
{
    using Show = FindClass1_t<Show, T>;

    if constexpr (Show{}) {
        return Show::type::show(instance);
    }
    else if constexpr (std::is_same_v<T, char>) {
        return std::string(1, instance);
    }
    else if constexpr (IsToStringable<T>{}) {
        return std::to_string(instance);
    }
    else if constexpr (std::is_same_v<T, std::string>) {
        return "\"" + ReplaceAll(instance, "\"", "\\\"") + "\"";
    }
    else {
        static_assert(CI::Traits::AlwaysFalse_v<Show>, "No instance found");
    }
}
