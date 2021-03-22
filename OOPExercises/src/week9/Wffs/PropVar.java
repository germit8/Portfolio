public class PropVar {
    
    private final String propName;

    public PropVar(String name) {
        this.propName = name;
    }

    @Override
    public String toString() {
        return propName;
    }
}
