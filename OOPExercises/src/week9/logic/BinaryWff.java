package logic;

public abstract class BinaryWff extends Wff {

    public abstract Operator getOperator();

    public abstract Wff getLeftWff();

    public abstract Wff getRightWff();

    public abstract String toString();
}
