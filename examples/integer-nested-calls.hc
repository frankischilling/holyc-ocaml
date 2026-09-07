I64 Add(I64 a, I64 b) {
    I64 c = a + b;
    return c;
}
I64 Twice(I64 n) {
    return Add(n, n);
}
I64 Sum(I64 n) {
    I64 total = 0;
    while (n > 0) {
        total = Add(total, n);
        n = n - 1;
    }
    return total;
}
(Sum(7) + Twice(7));
