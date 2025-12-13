# Vortex tests

add_executable(test_matula test_matula.c)
target_link_libraries(test_matula vortex-static m)

add_test(NAME matula_basic COMMAND test_matula)
