public class Sieve {
    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);
        boolean[] primes = new boolean[n];
        
        for (int k = 0; k < primes.length; k++) {
            primes[k] = true;
        }

        for (int i = 2; i < primes.length; i++) {
            if (primes[i]) {
                System.out.print(i + " ");

                for (int j = i*i; j < primes.length; j += i) {
                    primes[j] = false;
                }
            }
        }
    }
}
