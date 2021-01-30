function calculateCIfRightAngle() {
	var a = document.getElementById("numA").value;
	var b = document.getElementById("numB").value;
	var alfa = document.getElementById("numAlfa").value;
	var gamma = document.getElementById("numGamma").value;

	if (alfa == 90) {
		if (a != null && b != null) {
			document.getElementById("resultC").value = Math.sqrt(a ** 2) + b ** 2;
			} else {
				alert("Set a and b to a value!")
			}
		}
	if (alfa != 90) {
		if (a != null && gamma != null) {
			document.getElementById("resultC").value = a * Math.sin(gamma) / Math.sin(alfa);
		}
	}
}