// arrow funkce na generování náhodného čísla
const generateRandomNumber = () => {
    let minValue = Number(document.getElementById("min").value);
    let maxValue = Number(document.getElementById("max").value);
    return Math.floor(Math.random() * (maxValue - minValue + 1) + minValue)
}

// funkce na kontrolu inputů max a min value a výběr typ hry
function checkAndStart() {
    let minValue = Number(document.getElementById("min").value);
    let maxValue = Number(document.getElementById("max").value);
    let choice;
    if (minValue && maxValue != "") {
        choice = prompt("Press H for human guessing and C for computer!").toLowerCase()
        if (choice === "h") {
            gameLoop()
        } else if (choice === "c") {
            autoGameLoop()
        } else {
            alert("Wrong input, try again!")
        }
    } else {
        alert("Input minimal and maximal range!")
    }
}

// logika hry když hraje hráč
function gameLoop() {
    let minValue = Number(document.getElementById("min").value);
    let maxValue = Number(document.getElementById("max").value);
    let computerChosenValue = generateRandomNumber();
    let nickname = document.getElementById("nickname").value;
    let guessedNumber;
    let numberOfGuesses = 0;

    alert("Welcome " + nickname + " to number guessing! The computer has chosen a number in range from " + minValue + " to " + maxValue);

    while(guessedNumber !== computerChosenValue) {
        guessedNumber = Number(prompt("Guess a number: "));

        if (guessedNumber == "") {
            alert("Invalid input!")
        } else if (guessedNumber === computerChosenValue) {
            numberOfGuesses += 1;
            alert("You have guessed the number! It took you " + numberOfGuesses + " guesses")
            alert("To start another game, press the start button!")
        } else if (guessedNumber >= computerChosenValue) {
            alert("The number you are looking for is smaller!")
            numberOfGuesses += 1;
        } else if (guessedNumber <= computerChosenValue) {
            alert("The number you are looking for is greater!")
            numberOfGuesses += 1;
        }
    }
}

// logika hry když hraje počítač
function autoGameLoop() {
    let maxValue = Number(document.getElementById("max").value);
    let minValue = Number(document.getElementById("min").value);
    let computerChosenValue = generateRandomNumber();
    let guessedNumber;
    let numberOfGuesses = 0;

    alert("The computer is starting to guess!")
    alert("Number the computer has to guess: " + computerChosenValue)
    
    while (guessedNumber !== computerChosenValue) {
        guessedNumber = Math.floor((maxValue + minValue) / 2);
        alert("Number of total guesses yet: " + numberOfGuesses)
        alert("Guessing number: " + guessedNumber)

        if (guessedNumber === computerChosenValue) {
            alert("The computer has guessed the number! It took him " + numberOfGuesses + " guesses")
            alert("To start another game, press the start button!")
        } else if (guessedNumber >= computerChosenValue) {
            numberOfGuesses += 1
            maxValue = guessedNumber - 1
        } else if (guessedNumber <= computerChosenValue) {
            numberOfGuesses += 1
            minValue = guessedNumber + 1
        }
    }
}