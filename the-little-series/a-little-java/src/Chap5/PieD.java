package Chap5;

abstract class PieD {
//    RemV rFn = new RemV();
//    SubstV substFn = new SubstV();
//    abstract PieD rem(PieVisitorI pvFn);
//    abstract PieD subst(PieVisitorI pvFn);
    abstract PieD accept(PieVisitorI ask);
}

class Bot extends PieD {
    @Override
    PieD accept(PieVisitorI ask) {
        return ask.forBot();
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "()";
    }
}

class Top extends PieD {
    Object t;
    PieD r;
    Top(Object _t, PieD _r) {
        t = _t;
        r = _r;
    }

    @Override
    PieD accept(PieVisitorI ask) {
        return ask.forTop(t, r);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + t + ", " + r +")";
    }
}

abstract class FishD {}
class Anchovy extends FishD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Anchovy);
    }
}
class Salmon extends FishD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Salmon);
    }
}
class Tuna extends FishD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Tuna);
    }
}

interface PieVisitorI {
    PieD forBot();
    PieD forTop(Object t, PieD r);
}

class RemV implements PieVisitorI{
    Object o;
    RemV(Object _o) {
        o = _o;
    }
    public PieD forBot() {
        return new Bot();
    }
    public PieD forTop(Object t, PieD r) {
        if (o.equals(t))
            return r.accept(this);
        else
            return new Top(t, r.accept(this));
    }
}

abstract class SubstD implements PieVisitorI {
    Object n, o;

    SubstD(Object _n, Object _o) {
        n = _n;
        o = _o;
    }

    @Override
    public PieD forBot() {
        return new Bot();
    }

    @Override
    abstract public PieD forTop(Object t, PieD r);
}

class SubstV extends SubstD {
    SubstV(Object _n, Object _o) {
        super(_n, _o);
    }
    public PieD forTop(Object t, PieD r) {
        if (o.equals(t))
            return new Top(n, r.accept(this));
        else
            return new Top(t, r.accept(this));
    }
}

class LtdSubstV extends SubstD {
    int c;
    LtdSubstV(int _c, Object _n, Object _o) {
        super(_n, _o);
        c = _c;
    }

    @Override
    public PieD forTop(Object t, PieD r) {
        if (c == 0)
            return new Top(t, r);
        else
            if (o.equals(t))
                return new Top(n, r.accept(new LtdSubstV(c-1,n,o)));
            else
                return new Top(t, r.accept(this));
    }
}
