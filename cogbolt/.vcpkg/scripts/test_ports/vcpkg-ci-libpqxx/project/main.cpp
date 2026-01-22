#include <pqxx/pqxx>
int main()
{
pqxx::connection cx{"postgresql:
pqxx::work tx{cx};
return 0;
}