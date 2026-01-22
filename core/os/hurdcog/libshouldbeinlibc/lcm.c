static inline long
gcd (long p, long q)
{
if (p == 0)
return q;
else if (q == 0)
return p;
else if (p == q)
return p;
else if (q > p)
return gcd (q, p);
else
return gcd (q, p % q);
}
long
lcm (long p, long q)
{
return (p / gcd (p, q)) * q;
}