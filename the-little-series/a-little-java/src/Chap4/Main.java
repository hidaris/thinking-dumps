package Chap4;

class Main {
    public static void main(String args[]) {
        boolean s = new Onion(new Onion(new Skewer())).onlyOnions();
        System.out.println(s);
    }
}