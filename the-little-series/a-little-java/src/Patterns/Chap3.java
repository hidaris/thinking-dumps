package Patterns;
// This is similar to the interpreter and composite patterns.

abstract class PizzaD {
    RemAV raFn = new RemAV();
    TopAwCV topFn = new TopAwCV();
    SubstAwCV saFn = new SubstAwCV();
    abstract PizzaD remA();
    abstract PizzaD topAwC();
    abstract PizzaD subAbC();
}

class Crust extends PizzaD {
    @Override
    PizzaD remA() {
        return raFn.forCrust();
    }

    @Override
    PizzaD topAwC() {
        return topFn.forCrust();
    }

    @Override
    PizzaD subAbC() {
        return saFn.forCrust();
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "()";
    }
}

class Cheese extends PizzaD {
    PizzaD p;
    Cheese(PizzaD _p) {
        p = _p;
    }

    @Override
    PizzaD remA() {
        return raFn.forCheese(p);
    }

    @Override
    PizzaD topAwC() {
        return topFn.forCheese(p);
    }

    @Override
    PizzaD subAbC() {
        return saFn.forCheese(p);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + p + ")";
    }
}

class Olive extends PizzaD {
    PizzaD p;
    Olive(PizzaD _p) {
        p = _p;
    }

    @Override
    PizzaD remA() {
        return raFn.forOlive(p);
    }

    @Override
    PizzaD topAwC() {
        return topFn.forOlive(p);
    }

    @Override
    PizzaD subAbC() {
        return saFn.forOlive(p);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + p + ")";
    }
}

class Anchovy extends PizzaD {
    PizzaD p;
    Anchovy(PizzaD _p) {
        p = _p;
    }

    @Override
    PizzaD remA() {
        return raFn.forAnchovy(p);
    }

    @Override
    PizzaD topAwC() {
        return topFn.forAnchovy(p);
    }

    @Override
    PizzaD subAbC() {
        return saFn.forAnchovy(p);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + p + ")";
    }
}

class Sausage extends PizzaD {
    PizzaD p;
    Sausage(PizzaD _p) {
        p = _p;
    }

    @Override
    PizzaD remA() {
        return raFn.forSausage(p);
    }

    @Override
    PizzaD topAwC() {
        return topFn.forSausage(p);
    }

    @Override
    PizzaD subAbC() {
        return saFn.forSausage(p);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + p + ")";
    }
}

class Spinach extends PizzaD {
    PizzaD p;
    Spinach(PizzaD _p) {
        p = _p;
    }

    @Override
    PizzaD remA() {
        return raFn.forSpinach(p);
    }

    @Override
    PizzaD topAwC() {
        return topFn.forSpinach(p);
    }

    @Override
    PizzaD subAbC() {
        return saFn.forSpinach(p);
    }

    @Override
    public String toString() {
        return "new " + getClass().getName() + "(" + p + ")";
    }
}

class RemAV {
    PizzaD forCrust() {
        return new Crust();
    }
    PizzaD forCheese(PizzaD p) {
        return new Cheese(p.remA());
    }
    PizzaD forOlive(PizzaD p) {
        return new Olive(p.remA());
    }
    PizzaD forAnchovy(PizzaD p) {
        return p.remA();
    }
    PizzaD forSausage(PizzaD p) {
        return new Sausage(p.remA());
    }
    PizzaD forSpinach(PizzaD p) {
        return new Spinach(p.remA());
    }
}

class TopAwCV {
    PizzaD forCrust() {
        return new Crust();
    }
    PizzaD forCheese(PizzaD p) {
        return new Cheese(p.topAwC());
    }
    PizzaD forOlive(PizzaD p) {
        return new Olive(p.topAwC());
    }
    PizzaD forAnchovy(PizzaD p) {
        return new Cheese(new Anchovy(p.topAwC()));
    }
    PizzaD forSausage(PizzaD p) {
        return new Sausage(p.topAwC());
    }
    PizzaD forSpinach(PizzaD p) {
        return new Spinach(p.topAwC());
    }
}

class SubstAwCV {
    PizzaD forCrust() {
        return new Crust();
    }
    PizzaD forCheese(PizzaD p) {
        return new Cheese(p.subAbC());
    }
    PizzaD forOlive(PizzaD p) {
        return new Olive(p.subAbC());
    }
    PizzaD forAnchovy(PizzaD p) {
        return new Cheese(p.subAbC());
    }
    PizzaD forSausage(PizzaD p) {
        return new Sausage(p.subAbC());
    }
    PizzaD forSpinach(PizzaD p) {
        return new Spinach(p.subAbC());
    }
}