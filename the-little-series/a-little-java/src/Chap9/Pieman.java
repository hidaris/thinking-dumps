package Chap9;


abstract class PieD {
    abstract Object accept(PieVisitorI ask);
}

class Bot extends PieD {
    @Override
    Object accept(PieVisitorI ask) {
        return ask.forBot(this);
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
    Object accept(PieVisitorI ask) {
        return ask.forTop(this);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + t + ", " + r +")";
    }
}

interface PiemanI {
    int addTop(Object t);
    int remTop(Object t);
    int substTop(Object n, Object o);
    int occTop(Object o);
}

class PiemanM implements PiemanI {
    PieD p = new Bot();

    @Override
    public int addTop(Object t) {
        p = new Top(t, p);
        return occTop(t);
    }

    @Override
    public int remTop(Object t) {
        p = (PieD)p.accept(new RemV(t));
        return occTop(t);
    }

    @Override
    public int substTop(Object n, Object o) {
        p = (PieD)p.accept(new SubstV(n, o));
        return occTop(n);
    }

    @Override
    public int occTop(Object o) {
        return ((Integer)p.accept(new OccursV(o))).intValue();
    }
}


interface PieVisitorI {
    Object forBot(Bot that);
    Object forTop(Top that);
}

class RemV implements PieVisitorI{
    Object o;
    RemV(Object _o) {
        o = _o;
    }
    public Object forBot(Bot that) {
        return new Bot();
    }
    public Object forTop(Top that) {
        if (o.equals(that))
            return that.r.accept(this);
        else
            return new Top(that, (PieD) that.r.accept(this));
    }
}

abstract class SubstD implements PieVisitorI {
    Object n, o;

    SubstD(Object _n, Object _o) {
        n = _n;
        o = _o;
    }

    @Override
    public Object forBot(Bot that) {
        return that;
    }

    @Override
    abstract public Object forTop(Top that);
}

class SubstV extends SubstD {
    SubstV(Object _n, Object _o) {
        super(_n, _o);
    }
    public Object forTop(Top that) {
        if (o.equals(that)) {
            that.t = n;
            that.r.accept(this);
            return that; }
        else {
            that.r.accept(this);
            return that;
        }
    }
}

class LtdSubstV extends SubstD {
    int c;
    LtdSubstV(int _c, Object _n, Object _o) {
        super(_n, _o);
        c = _c;
    }

    @Override
    public Object forTop(Top that) {
        if (c == 0)
            return that;
        else
        if (o.equals(that)) {
            that.t = n;
            that.r.accept(new LtdSubstV(c-1,n,o));
            return that;
        }
        else {
            that.r.accept(this);
            return that;
        }
    }
}

class OccursV implements PieVisitorI {
    Object a;
    OccursV(Object _a) {
        a = _a;
    }
    public Object forBot(Bot that) {
        return new Integer(0);
    }
    public Object forTop(Top that) {
        if (that.t.equals(a))
            return new Integer(((Integer)(that.r.accept(this))).intValue()
                    +
                    1);
        else
            return that.r.accept(this);
    }
}
