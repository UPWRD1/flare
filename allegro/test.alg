-- we love comments

op factorial: Int of (n: Int) -> {
    return n * factorial(n - 1)
}

op main: .. of () -> {
    let x := factorial(10);
    print x;
}
 