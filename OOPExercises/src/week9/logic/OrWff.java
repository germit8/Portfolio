package logic;

public class OrWff extends BinaryWff {

    public OrWff(PropVar left, PropVar right) {
        super(left, right);
        setOperator(Operator.OR);
    }
}
