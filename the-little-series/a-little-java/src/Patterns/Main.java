package Patterns;

class Main {
    public static void main(String args[]) {
        NumD zero = new Zero();
        NumD one = new OneMoreThan(zero);
        NumD two = new OneMoreThan(one);
        LayerD l = new Base(5);
        PizzaD p = (new Olive(new Anchovy(new Cheese(new Anchovy(new Crust())))).remA()).topAwC();
        PizzaD p2 = (new Olive(new Anchovy(new Cheese(new Anchovy(new Crust())))).topAwC()).remA();
        System.out.println(p);
        System.out.println(p2);
    }
}