#include <iostream>
#include <string>
#include "myvector.h"
#include "typeclasses/ShowIO.h"

int main(void)
{
    struct MovableBox
    {
        MovableBox(int a) : m_val(a) {}
        MovableBox(MovableBox&& o) noexcept : m_val(o.m_val)
        {
            o.m_val = 0;
            std::cout << "Moved " << m_val << std::endl;
        }
        MovableBox(const MovableBox& o) : m_val(o.m_val)
        {
            std::cout << "Copied " << m_val << std::endl;
        }
        MovableBox& operator=(const MovableBox&) = default;
        MovableBox& operator=(MovableBox&&) = default;
        operator int() const { return m_val; }

        int m_val;
    };

    std::vector<MovableBox> a;
    MovableBox test1('a');
    MovableBox test2('b');
    a.push_back(test1);
    a.push_back(test2);

    std::cout << "==========Begin Map==========" << std::endl;

    //std::cout << a << std::endl;

    //// copy identity map
    //std::cout << fmap(a, [](auto&& i) { return i; }) << std::endl;
    //std::cout << a << std::endl;

    //// move identity map
    //std::cout << fmap(std::move(a), [](auto&& i) { return i; }) << std::endl;
    //std::cout << a << std::endl;

    //// copy change type
    //a.push_back(test1);
    //a.push_back(test2);
    //std::cout << fmap(a, [](auto&& i) { return static_cast<char>(i); }) << std::endl;

    //// move change type
    //std::cout << fmap(std::move(a), [](auto&& i) { return static_cast<char>(i); }) << std::endl;
    //std::cout << a << std::endl;

    return 0;
}
