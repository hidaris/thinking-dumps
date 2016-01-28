package Chap6;

abstract class FruitD {}

class Peach extends FruitD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Peach);
    }
}

class Apple extends FruitD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Apple);
    }
}

class Pear extends FruitD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Pear);
    }
}

class Lemon extends FruitD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Lemon);
    }
}

class Fig extends FruitD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Fig);
    }
}

interface ITree<A> {
    <A> A accept(TreeVisitorI<A> ask);
}

class Bud<A> implements ITree<A> {
    @Override
    public <A> A accept(TreeVisitorI<A> ask) {
        return ask.forBud();
    }
}

class Flat<A> implements ITree<A> {
    FruitD f;
    ITree t;
    Flat(FruitD _f, ITree _t) {
        f = _f;
        t = _t;
    }

    @Override
    public <A1> A1 accept(TreeVisitorI<A1> ask) {
        return ask.forFlat(f, t);
    }
}

class Split<A> implements ITree<A> {
    ITree l;
    ITree r;
    Split(ITree _l, ITree _r) {
        l = _l;
        r = _r;
    }

    @Override
    public <A1> A1 accept(TreeVisitorI<A1> ask) {
        return ask.forSplit(l, r);
    }
}

interface TreeVisitorI<A> {
    A forBud();
    A forFlat(FruitD f, ITree t);
    A forSplit(ITree l, ITree r);
}

class isFlatV implements TreeVisitorI<Boolean> {
    @Override
    public Boolean forBud() {
        return new Boolean(true);
    }

    @Override
    public Boolean forFlat(FruitD f, ITree t) {
        return (Boolean) t.accept(this);
    }

    @Override
    public Boolean forSplit(ITree l, ITree r) {
        return new Boolean(false);
    }
}

class isSplitV implements TreeVisitorI<Boolean> {
    @Override
    public Boolean forBud() {
        return new Boolean(true);
    }

    @Override
    public Boolean forFlat(FruitD f, ITree t) {
        return new Boolean(false);
    }

    @Override
    public Boolean forSplit(ITree l, ITree r) {
        if ((Boolean)(l.accept(this)))
            return (Boolean) r.accept(this);
        else
            return new Boolean(false);
    }
}

class hasFruitV implements TreeVisitorI<Boolean> {
    @Override
    public Boolean forBud() {
        return new Boolean(false);
    }

    @Override
    public Boolean forFlat(FruitD f, ITree t) {
        return new Boolean(true);
    }

    @Override
    public Boolean forSplit(ITree l, ITree r) {
        if ((Boolean)(l.accept(this)))
            return new Boolean(true);
        else
            return (Boolean) r.accept(this);
    }
}

class iHeightV implements TreeVisitorI<Integer> {
    @Override
    public Integer forBud() {
        return 0;
    }

    @Override
    public Integer forFlat(FruitD f, ITree t) {
        return ((Integer)(t.accept(this))) + 1;
    }

    @Override
    public Integer forSplit(ITree l, ITree r) {
        return Math.max(((Integer)(l.accept(this))), ((Integer)(r.accept(this)))) + 1;
    }
}

class tSubstV implements TreeVisitorI<ITree> {
    FruitD n;
    FruitD o;
    tSubstV(FruitD _n, FruitD _o) {
        n = _n;
        o = _o;
    }

    @Override
    public ITree forBud() {
        return new Bud();
    }

    @Override
    public ITree forFlat(FruitD f, ITree t) {
        if (o.equals(f))
            return new Flat(n, (ITree) t.accept(this));
        else
            return new Flat(f, (ITree) t.accept(this));
    }

    @Override
    public ITree forSplit(ITree l, ITree r) {
        return new Split((ITree)l.accept(this),
                         (ITree) r.accept(this));
    }
}

class iOccursV implements TreeVisitorI<Integer> {
    FruitD a;
    iOccursV(FruitD _a) {
        a = _a;
    }

    @Override
    public Integer forBud() {
        return 0;
    }

    @Override
    public Integer forFlat(FruitD f, ITree t) {
        if (f.equals(a))
            return (Integer)(t.accept(this)) + 1;
        else
            return (Integer) t.accept(this);
    }

    @Override
    public Integer forSplit(ITree l, ITree r) {
        return (Integer) l.accept(this) + (Integer) r.accept(this);
    }
}