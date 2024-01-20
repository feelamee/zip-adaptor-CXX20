#include <algorithm>
#include <concepts>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

namespace detail
{

template <typename... Args>
struct tuple_or_pair
{
    using type = std::tuple<Args...>;
};

template <typename Arg1, typename Arg2>
struct tuple_or_pair<Arg1, Arg2>
{
    using type = std::pair<Arg1, Arg2>;
};

template <typename... Args>
using tuple_or_pair_t = tuple_or_pair<Args...>::type;

template <bool is_const, typename T>
using maybe_const = std::conditional_t<is_const, const T, T>;

template <bool is_const, std::ranges::input_range... Rngs>
constexpr auto
iterator_tag(Rngs&&...)
{

    if constexpr ((std::ranges::contiguous_range<maybe_const<is_const, Rngs>> &&
                   ...))
        return std::contiguous_iterator_tag{};
    else if constexpr ((std::ranges::random_access_range<
                            maybe_const<is_const, Rngs>> &&
                        ...))
        return std::random_access_iterator_tag{};
    else if constexpr ((std::ranges::bidirectional_range<
                            maybe_const<is_const, Rngs>> &&
                        ...))
        return std::bidirectional_iterator_tag{};
    else if constexpr ((std::ranges::forward_range<
                            maybe_const<is_const, Rngs>> &&
                        ...))
        return std::forward_iterator_tag{};
    else
        return std::input_iterator_tag{};
}

template <bool is_const, typename... Rngs>
using iterator_tag_t =
    decltype(iterator_tag<is_const>(std::declval<Rngs>()...));

template <typename Fn, typename Tuple>
auto
tuple_transform(Fn&& fn, Tuple&& tpl)
{
    return std::apply(
        [&]<typename... Args>(Args&&... args)
        {
            return tuple_or_pair_t<std::invoke_result_t<Fn&, Args>...>(
                std::invoke(fn, std::forward<Args>(args))...);
        },
        std::forward<Tuple>(tpl));
}

template <typename Fn, typename Tuple>
auto
tuple_for_each(Fn&& fn, Tuple&& tpl)
{
    std::apply([&](auto&&... args)
               { (std::invoke(fn, std::forward<decltype(args)>(args)), ...); },
               std::forward<Tuple>(tpl));
}

} // namespace detail

template <std::ranges::view... Vs>
    requires(std::ranges::input_range<Vs> && ...) && (sizeof...(Vs) > 0)
struct zip_impl : std::ranges::view_interface<zip_impl<Vs...>>

{
private:
    detail::tuple_or_pair_t<Vs...> views;

public:
    constexpr zip_impl(Vs... vs)
        : views{ std::move(vs)... }
    {
    }

    template <bool is_const>
    struct iterator
    {
        template <typename... Args>
        using tuple = detail::tuple_or_pair_t<Args...>;

        template <typename Arg>
        using mbconst = detail::maybe_const<is_const, Arg>;

        using iterator_category = detail::iterator_tag_t<is_const, Vs...>;
        using difference_type =
            std::common_type_t<std::ranges::range_difference_t<mbconst<Vs>>...>;
        using value_type = tuple<std::ranges::range_value_t<mbconst<Vs>>...>;
        using reference = tuple<std::ranges::range_reference_t<mbconst<Vs>>...>;
        using const_reference =
            tuple<const std::ranges::range_reference_t<mbconst<Vs>>...>;

        using self_type = iterator<is_const>;

        tuple<std::ranges::iterator_t<mbconst<Vs>>...> value;

        constexpr iterator() = default;
        constexpr iterator(decltype(value) iters)
            : value{ std::move(iters) }
        {
        }

        constexpr iterator(const self_type& iters)
            : value{ iters.value }
        {
        }

        // oh my god... this is work
        constexpr auto operator<=>(const self_type& rhs) const = default;

        // oh my god... all this handby
        constexpr reference
        operator[](const difference_type& n)
        {
            return detail::tuple_transform(
                [&](auto& arg) -> decltype(auto) { return arg[n]; }, value);
        }

        constexpr const_reference
        operator[](const difference_type& n) const
        {
            return detail::tuple_transform(
                [&](auto& arg) -> decltype(auto) { return arg[n]; }, value);
        }

        constexpr friend difference_type
        operator-(const self_type& lhs, const self_type& rhs)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            return std::distance(std::get<0>(rhs.value),
                                 std::get<0>(lhs.value));
        }

        constexpr self_type&
        operator+=(const difference_type& n)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            *this = *this + n;
            return *this;
        }

        constexpr self_type&
        operator-=(const difference_type& n)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            *this = *this - n;
            return *this;
        }

        constexpr friend self_type
        operator+(const self_type& lhs, const difference_type& n)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            auto it = lhs;
            detail::tuple_for_each([&](auto& args) { args = args + n; },
                                   it.value);
            return it;
        }

        constexpr friend self_type
        operator+(const difference_type& n, const self_type& lhs)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            return lhs + n;
        }

        constexpr friend self_type
        operator-(const self_type& lhs, const difference_type& n)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            return lhs - n;
        }

        constexpr friend self_type
        operator-(const difference_type& n, const self_type& lhs)
            requires std::is_base_of_v<std::random_access_iterator_tag,
                                       iterator_category>
        {
            return lhs - n;
        }

        constexpr self_type&
        operator--()
            requires std::is_base_of_v<std::bidirectional_iterator_tag,
                                       iterator_category>
        {
            detail::tuple_for_each([&](auto& args) { --args; }, value);
            return *this;
        }

        constexpr self_type
        operator--(int)
            requires std::is_base_of_v<std::bidirectional_iterator_tag,
                                       iterator_category>
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        constexpr self_type&
        operator++()
            requires std::is_base_of_v<std::input_iterator_tag,
                                       iterator_category>
        {
            detail::tuple_for_each([&](auto& args) { ++args; }, value);
            return *this;
        }

        constexpr self_type
        operator++(int)
            requires std::is_base_of_v<std::input_iterator_tag,
                                       iterator_category>
        {
            auto tmp = *this;
            ++*this;
            return tmp;
        }

        constexpr reference
        operator*()
        {
            return detail::tuple_transform(
                [&](auto& arg) -> decltype(auto) { return *arg; }, value);
        }

        constexpr reference
        operator*() const
        {
            return detail::tuple_transform(
                [&](auto& arg) -> decltype(auto) { return *arg; }, value);
        }
    };

    constexpr auto
    begin() const
    {
        return iterator<true>{ detail::tuple_transform(std::ranges::begin,
                                                       views) };
    }
    constexpr auto
    begin()
    {
        return iterator<false>{ detail::tuple_transform(std::ranges::begin,
                                                        views) };
    }

    constexpr auto
    end() const
    {
        return iterator<true>{ detail::tuple_transform(
            [&](auto& arg)
            { return std::ranges::next(std::ranges::begin(arg), size()); },
            views) };
    }

    constexpr auto
    end()
    {
        return iterator<false>{ detail::tuple_transform(
            [&](auto& arg)
            { return std::ranges::next(std::ranges::begin(arg), size()); },
            views) };
    }

    constexpr auto
    size() const
        requires(std::ranges::sized_range<Vs> && ...)
    {
        return std::apply(
            [&](auto&&... args)
            { return std::ranges::min({ std::ranges::size(args)... }); },
            views);
    }
};

struct Zip
{
    template <typename... Vs>
    constexpr auto
    operator()(Vs&&... views) const
    {
        if constexpr (0 == sizeof...(views))
            return std::views::empty<std::tuple<>>;
        else
            return zip_impl<std::views::all_t<Vs>...>(
                std::forward<Vs>(views)...);
    }
};
inline constexpr Zip zip;

TEST_CASE("test zip impl")
{
    std::vector tmp1{ 1, 2, 3, 5, 6 };
    std::vector tmp2{ 15, 52, 123, 213, 123 };
    std::vector tmp3{ "4", "fldsajkf", "fg", "41", "13" };

    static_assert(std::ranges::random_access_range<decltype(zip(tmp2, tmp3))>);

    const int skip{ 0 };
    auto it{ tmp1.begin() + skip };
    auto jt{ tmp2.begin() + skip };
    auto kt{ tmp3.begin() + skip };

    for (auto [i, j, k] :
         zip(tmp1, tmp2, tmp3) | std::views::take(3) | std::views::drop(skip))
    {
        std::cout << i << " " << j << " " << k << std::endl;
        REQUIRE(i == *it);
        REQUIRE(j == *jt);
        REQUIRE(k == *kt);
        ++it;
        ++jt;
        ++kt;
    }
}