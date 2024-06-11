op main: .. of () -> {
    let a := 3 < 10; -- 1
    let b := 3 < 3; -- 0
    let c := 3 <= 3; -- 1
    let d := 3 > 1; -- 1
    let e := 3 > 3; -- 0
    let f := 3 >= 3; -- 1

    print a;
    print b;
    print c;
    print d;
    print e;
    print f;
}
