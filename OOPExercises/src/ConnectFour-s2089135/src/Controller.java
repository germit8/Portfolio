/**
 * This file is to be completed by you.
 *
 * @author <s2089135>
 */

public final class Controller {
	private final Model model;
	private final TextView view;
	
	public Controller(Model model, TextView view) {
		this.model = model;
		this.view = view;
	}

	// function that pauses the program for certain amount of miliseconds
	// I am not sure if I can use something like this, but since it does not really change anything
	// about the code functionally, I assumed I could give it a try - it merely simulates more "realistic"
	// process of the game, because everything doesn't happen so suddenly
	public static void wait(int ms) {
    	try {
        	Thread.sleep(ms);
    	} catch(InterruptedException ex) {
        	Thread.currentThread().interrupt();
    	}
	}
	
	public void startSession()
	{
		// ====================================================================================
		// ================================ INITIALIZATION OF GAME ============================
		// ====================================================================================
		
		// If we load game, then everything is set and we do not go through initialization
		boolean gameLoaded = false;
		if (view.askForLoad() == 'y') {
			gameLoaded = model.loadGame();
			if (!(gameLoaded)) {
				view.displayLoadErrorMessage();
			}
		} 
		if (!(gameLoaded)) { 
			model.setPlayer1(view.askForName());
			
			// AI/Player 2 setting
			if (view.askForPCMode() == 'y') {
				model.setPC(true);
				model.setPlayer2("Hal");
			} else {
				model.setPlayer2(view.askForName2()); 
			}
			// Option to adjust some settings - if not used, all values are set to standard by model constructor
			if (view.askForSettings() == 'y') {
				model.setRows(view.askForRows());
				model.setCols(view.askForCols());
				model.setConnectN(view.askForConnectN(model.getNrRows(), model.getNrCols()));
				model.setToken1(view.askForP1Token(model.getPlayer1()));
				if (model.getPC()) {
					model.setToken2(view.askForAIToken(model.getP1Token()));
				} else {
					model.setToken2(view.askForP2Token(model.getPlayer2(), model.getP1Token()));
				}
				model.setGrid();
			}
		}
		int boardSpace = model.getNrCols() * model.getNrRows();

		view.displayBoard(model);
		view.displayNewGameMessage();

		// ====================================================================================
		// ================================ GAME LOOP =========================================
		// ====================================================================================
		
		// Odd turns are always for player 1, even turns are either for player 2 or AI, based on boolean pcMode
		while (model.getTurn() <= boardSpace && !(model.getWin())) {
			view.displaySaveMessage();
			view.displayLoadMessage();
			view.displayForfeitMessage();

			if (model.getTurn() % 2 != 0) {
				view.displayTurn(model.getPlayer1());
				int newMove = view.askForMove();
	
				// loop that checks if newMove is 0 or -1 - this way player can save/load how many times they want in a row without breaking the game
				while (newMove == 0 || newMove == -1) {
					if (newMove == 0) {
						model.createSaveFile();		
						if (model.saveGame()) {
							view.displayGameSavedMessage();
						}
						wait(1000);					
					} else if (newMove == -1) {
						view.displayGameLoading();
						wait(1000);
						if (model.loadGame()) {
							view.displayLoadedData(model);
							wait(1000);
							view.displayGameLoadedMessage();
						}
						view.displayBoard(model);
						view.displaySaveMessage();
						view.displayLoadMessage();
						view.displayForfeitMessage();
						view.displayTurn(model.getPlayer1());
					} 
					newMove = view.askForMove();
				}

				// Making an actual move
				if (model.isMoveValid(newMove)) {
					model.makeMove(newMove);
				// Forfeit option
				} else if (newMove == -2) {							
					model.setWin(true);
					// We have to subtract turn here to get the correct winner, because turn always increments after it runs to the end of the while loop
					model.changeTurn(-1);
				} else {
					view.displayInvalidMove();
					model.changeTurn(-1);
					wait(1000);
				}
			// Player 2 turn, basically the same code as for player 1
			} else if (model.getTurn() % 2 == 0 && !(model.getPC())) {
				view.displayTurn(model.getPlayer2());
				int newMove = view.askForMove();
	
				while (newMove == 0 || newMove == -1) {
					if (newMove == 0) {
						model.createSaveFile();		
						if (model.saveGame()) {
							view.displayGameSavedMessage();
						}
						wait(1000);					
					} else if (newMove == -1) {
						view.displayGameLoading();
						wait(1000);
						if (model.loadGame()) {
							view.displayLoadedData(model);
							wait(1000);
							view.displayGameLoadedMessage();
						}
						view.displayBoard(model);
						view.displaySaveMessage();
						view.displayLoadMessage();
						view.displayForfeitMessage();
						view.displayTurn(model.getPlayer2());
					}
					newMove = view.askForMove();
				}
				if (model.isMoveValid(newMove)) {
					model.makeMove(newMove);
				} else if (newMove == -2) {							
					model.setWin(true);
					model.changeTurn(-1);
				} else {
					view.displayInvalidMove();
					model.changeTurn(-1);
					wait(1000);
				}
			} else {
				view.displayTurn(model.getPlayer2());
				wait(1000);
				// Computer will be making attempts until it is successful
				boolean computerMove = model.computerMakeMove();
				while (computerMove == false) {
					computerMove = model.computerMakeMove();
				}
			}
			
			view.displayBoard(model);

			// Checking for win/draw
			if (model.getWin()) {
				if (model.getTurn() % 2 != 0) {
					view.displayWin(model.getPlayer1());
				} else {
					view.displayWin(model.getPlayer2());
				}
			} else if (model.getTurn() == boardSpace) {
				view.displayDrawMessage();
			}

			model.changeTurn(1);
		}

		if (view.askForNewGame() == 'y') {
			model.resetGame();
			startSession();
		} else {
			view.displayThankYou();
		}
	}
}
