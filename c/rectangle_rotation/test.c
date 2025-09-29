#include <criterion/criterion.h>

long long rectangle_rotation(int a, int b);

Test(solution_test, basic_tests)
{
    cr_assert_eq(rectangle_rotation(6, 4), 23);
    cr_assert_eq(rectangle_rotation(30, 2), 65);
    cr_assert_eq(rectangle_rotation(8, 6), 49);
    cr_assert_eq(rectangle_rotation(16, 20), 333);
}
