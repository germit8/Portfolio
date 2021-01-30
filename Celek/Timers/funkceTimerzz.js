let timeFlow = 5;
let timer;
let clockTimer;
let randomNumber1;
let randomNumber2;
let randomMathOperation;
let randomOperator;
let randomStringText;

/* funkce na zvyšování a zobrazování časovače na stránce 
kontrola vypršení času při výpočtech a reset*/
function timeIncrement() {
    let counterColor = document.querySelector("#output");
    timeFlow -= 1;
    document.getElementById("output").innerHTML = timeFlow;

    if (timeFlow <= 2) {
        counterColor.style.color = "red";
    }

    if (timeFlow == 0) {
        stopTime();
        document.getElementById("resultMessage").innerHTML = "You ran out of time! Try again.";
        document.getElementById("resultNumber").value = null;
        timeFlow = 5;
        document.getElementById("output").innerHTML = `${timeFlow}`;
        setTimeout(resetIt, 2000);
    }
}

// funkce na nastavení intervalu časovače
function autoTimer() {
    timer = setInterval(timeIncrement, 1000);
}

// funkce na zastavení časovače
function stopTime() {
	clearInterval(timer);
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

    switch(randomMathOperation) {
        case 1:
            randomOperator = " + ";
            break;
        case 2:
            randomOperator = " - ";
            break;
        case 3:
            randomOperator = " * ";
            break;
        case 4:
            randomOperator = " / ";
            break;
    }

    randomStringText = (`${randomNumber1} ${randomOperator} ${randomNumber2}`);

    document.getElementById("operationOfRandomNumbers").innerHTML = randomStringText;
    document.getElementById("resultNumber").focus();

    autoTimer();
}

// funkce na kontrolu výsledku a zastavení času po potvrzení zadané hodnoty
function checkIt() {
	let humanGuess = document.getElementById("resultNumber").value;
    let randomOperation;
    
    switch(randomOperator) {
        case " + ":
            randomOperation = randomNumber1 + randomNumber2;
            break;
        case " - ":
            randomOperation = randomNumber1 - randomNumber2;
            break;
        case " * ":
            randomOperation = randomNumber1 * randomNumber2;
            break;
        case " / ":
            randomOperation = randomNumber1 / randomNumber2;
            break;
    }

	if (humanGuess == randomOperation) {
        document.getElementById("resultMessage").innerHTML = "Congratulations! That is correct.";
        setTimeout(resetIt, 2000);
	} else {
        document.getElementById("resultMessage").innerHTML = "Incorrect. Try again.";
        setTimeout(resetIt, 2000);
    }

    stopTime();
}

// funkce na resetnutí parametrů stránky
function resetIt() {
    document.getElementById("resultMessage").innerHTML = "Result Message";
    document.getElementById("randomizeNumber").value = null;
    document.getElementById("operationOfRandomNumbers").innerHTML = "Operation";
    document.getElementById("resultNumber").value = null;
    document.querySelector("#output").style.color = "black";
    timeFlow = 5;
    document.getElementById("output").innerHTML = `${timeFlow}`;
}

// funkce na mačkání tlačítek
function pressKey(event) {
    let pressed = event.code;

    switch(pressed) {
        case "Enter":
            checkIt();
            break;
        case "Space":
            resetIt();
            break; 
    }
}

// funkce na fetch času z jiného zdroje času
function clockIncrement() {
    updateElementFromPlainText("http://ajax.damto.cz/textplain.php", "time");
}

// funkce na aktualizaci a obnovu času
function clockTime() {
    clockTimer = setInterval(clockIncrement, 1000);
}

// funkce na generování náhodných barev
function randomColorGenerator() {
    let letters = "0123456789ABCDEF";
    let color = "#";
    for (let i = 0; i < 6; i++) {
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color
}

// funkce na přiřazení barvy prvku
function colorChange() {
    let heading = document.querySelector("h1");
    let paragraph = document.querySelector("p");
    heading.style.color = randomColorGenerator();
    paragraph.style.color = randomColorGenerator();
}

// funkce na volání změny barvy v intervalu
function colorTiming() {
    setInterval(colorChange, 250);
}