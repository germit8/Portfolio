let timeFlow = 0;
let timer;
let clockTimer;
let randomNumber1;
let randomNumber2;
let randomMathOperation;
let randomOperator;
let randomStringText;

// funkce na zvyšování a zobrazování časovače na stránce
function timeIncrement() {
	timeFlow += 1;
	document.getElementById("output").innerHTML = timeFlow;
}

// funkce na počítání vteřin od spuštění stránky
function autoTimer() {
    timer = setInterval(timeIncrement, 1000);
    clockTimer = setInterval(clockTime, 1000);
}

// funkce na zastavení timeru
function stopTime() {
	clearInterval(timer);
}

// funkce na pokračování timeru
function continueTime() {
    autoTimer();
}

// funkce na generování náhodného čísla
function randomizeNumber() {
	let randomNumber = Math.round(Math.random() * 1000);
	document.getElementById("randomizeNumber").value = randomNumber;
}

// funkce na generaci dvou náhodných čísel pro matematické operace
function giveMeRandomNumbers() {
	randomNumber1 = Math.round(Math.random() * 100);
    randomNumber2 = Math.round(Math.random() * 100);
    randomMathOperation = Math.round(Math.random() * 4);

    if (randomMathOperation == 1) {
        randomOperator = " + ";
    } else if (randomMathOperation == 2) {
        randomOperator = " - ";
    } else if (randomMathOperation == 3) {
        randomOperator = " * ";
    } else {
        randomOperator = " / ";
    }

    randomStringText = (`${randomNumber1} ${randomOperator} ${randomNumber2}`);

    document.getElementById("operationOfRandomNumbers").innerHTML = randomStringText;
    document.getElementById("resultNumber").focus();
}

// funkce na kontrolu výsledku
function checkIt() {
	let humanGuess = document.getElementById("resultNumber").value;
    let randomOperation;
    
    if (randomOperator === " + ") {
        randomOperation = randomNumber1 + randomNumber2;
    } else if (randomOperator === " - ") {
        randomOperation = randomNumber1 - randomNumber2;
    } else if (randomOperator === " * ") {
        randomOperation = randomNumber1 * randomNumber2;
    } else {
        randomOperation = Math.round(randomNumber1 / randomNumber2);
    }
			
	if (humanGuess == randomOperation) {
		document.getElementById("resultMessage").innerHTML = "Congratulations! That is correct.";
	} else {
		document.getElementById("resultMessage").innerHTML = "Incorrect. Try again.";
	}
}

// funkce na clearnutí zprávy hodnotící výsledek a random čísla
function resetIt() {
    document.getElementById("resultMessage").innerHTML = "Result Message";
    document.getElementById("randomizeNumber").value = null;
    document.getElementById("operationOfRandomNumbers").innerHTML = "Operation";
    document.getElementById("resultNumber").value = null;
}

// funkce na mačkání tlačítek
function pressKey(event) {
    let pressed = event.code;

    if (pressed == "Enter") {
        checkIt();
    } 
    if (pressed == "Space") {
        resetIt();
    }
}

// funkce na běh času
function clockTime() {
    updateElementFromPlainText("http://ajax.damto.cz/textplain.php", "time");
}