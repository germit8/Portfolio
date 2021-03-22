public class BinaryWff {
    
    private final PropVar leftVar;
    private final PropVar rightVar;
    private Operator operator;

    public BinaryWff(PropVar left, PropVar right) {
        this.leftVar = left;
        this.rightVar = right;
    }

    public void setOperator(Operator operator) {
        this.operator = operator;
    }

    public Operator getOperator() {
        return operator;
    }

    public PropVar getLeft() {
        return leftVar;
    }

    public PropVar getRight() {
        return rightVar;
    }

    @Override
    public String toString() {
        return leftVar.toString() + " " + operator + " " + rightVar.toString();
    }
}
