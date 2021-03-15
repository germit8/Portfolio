/**
 * This file is to be completed by you.
 *
 * @author <s2089135>
 */

public final class Controller
{
	private final Model model;
	private final TextView view;
	
	public Controller(Model model, TextView view)
	{
		this.model = model;
		this.view = view;
	}

	// function that pauses the program for certain amount of miliseconds
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
		
		// If we load game, then everything is set and we do not have to go through initialization
		if (view.askForResponse("Would you like to load game? Y/N: ") == 'Y') {
			model.loadGame();
		} else { 
			model.setPlayer1(view.askForName("Hello, how would you like to be called? "));
			
			// AI/Player 2 setting
			if (view.askForResponse("Would you like to play against a computer? Y/N: ") == 'Y') {
				model.setPC(true);
				model.setPlayer2("Hal");
			} else {
				model.setPlayer2(view.askForName("How woud you like to be called player 2? ")); 
			}
			// Option to adjust some settings - if not used, all values are set to standard by model constructor
			if (view.askForResponse("Would you like to customise settings? Y/N: ") == 'Y') {
				model.setRows(view.askForDimension("Set rows to: "));
				model.setCols(view.askForDimension("Set columns to: "));
				model.setConnectN(view.askForDimension("How many tokens in row to win? "));
				model.setToken1(view.askForToken(model.getPlayer1() + " choose a token (any character except zero): "));
				if (model.getPC()) {
					model.setToken2(view.askForToken("Choose a token for computer (any character except zero): "));
				} else {
					model.setToken2(view.askForToken(model.getPlayer2() + " choose a token (any character except zero): "));
				}
				model.setGrid();
			}
		}
		int boardSpace = model.getNrCols() * model.getNrRows();
		System.out.println("Alrighty, let's start the game!");

		view.displayBoard(model);
		view.displayNewGameMessage();
		System.out.println("Board space: " + boardSpace);

		// If AI mode is on, AI game loop commences, otherwise human vs human game loop starts
		if (model.getPC()) {

			// ====================================================================================
			// ================================ AI GAME LOOP ======================================
			// ====================================================================================
			
			while (model.getTurn() <= boardSpace && !(model.getWin())) {

				// Players change with odd and even turns - odd is for player1, even for player2 (AI)
					if (model.getTurn() % 2 != 0) {
						System.out.println("It is " + model.getPlayer1() + "'s turn");
						System.out.println("TO SAVE GAME ENTER: 0");
						System.out.println("TO LOAD LAST GAME ENTER: -1");
						System.out.println("TO FORFEIT ENTER: -2");
						int newMove = view.askForMove();

						if (newMove == 0) {
							model.createSaveFile();
							wait(1000);
							model.saveGame();
							wait(1000);
							// after saving, this line below ensures asks for an actual move and then does it
							model.makeMove(view.askForMove());
						} else if (newMove == -1) {
							System.out.println("Game is loading...");
							wait(1000);
							model.loadGame();
							wait(1000);
						} else if (newMove == -2) {
							model.setWin(true);
							// We have to subtract turn here to get the correct winner
							model.changeTurn(-1);
						} else {
							// If none of the special interactions were chosen, we make the move
							model.makeMove(newMove);
						}
					} else {
						System.out.println("It is " + model.getPlayer2() + "'s turn");
						wait(1000);
						// Computer will be making attempts until it is successful (more info in Model)
						while (model.computerMakeMove() == false) {
							model.computerMakeMove();
						}
					}
	
					view.displayBoard(model);
	
					// Checking if the state of "win" was changed (operated in Model)
					if (model.getWin()) {
						if (model.getTurn() % 2 != 0) {
							System.out.println(model.getPlayer1() + " is the winner!");
						} else {
							System.out.println("Computer is the winner!");
						}
					} // Checking if board is full - game ends with a draw 
					else if (model.getTurn() == boardSpace) {
						System.out.println("You have filled the board - it is a draw!");
					}
	
					model.changeTurn(1);
				}
		} else {
			// ====================================================================================
			// ================================ PLAYER GAME LOOP ==================================
			// ====================================================================================
			
			while (model.getTurn() <= boardSpace && !(model.getWin())) {

			// Most of the code here is the same as in AI loop
				if (model.getTurn() % 2 != 0) {
					System.out.println("It is " + model.getPlayer1() + "'s turn");
				} else {
					System.out.println("It is " + model.getPlayer2() + "'s turn");
				}
				System.out.println("TO SAVE GAME ENTER: 0");
				System.out.println("TO LOAD LAST GAME ENTER: -1");
				System.out.println("TO FORFEIT ENTER: -2");

				int newMove = view.askForMove();
				if (newMove == 0) {
					model.createSaveFile();
					wait(1000);
					model.saveGame();
					wait(1000);
					model.makeMove(view.askForMove());
				} else if (newMove == -1) {
					System.out.println("Game is loading...");
					wait(1000);
					model.loadGame();
					wait(1000);
				} else if (newMove == -2) {
					model.setWin(true);
					model.changeTurn(-1);
				} else {
					model.makeMove(newMove);
				}

				view.displayBoard(model);

				if (model.getWin()) {
					if (model.getTurn() % 2 != 0) {
						System.out.println(model.getPlayer1() + " is the winner!");
					} else {
						System.out.println(model.getPlayer2() + " is the winner!");
					}
				} else if (model.getTurn() == boardSpace) {
					System.out.println("You have filled the board - it is a draw!");
				}

				model.changeTurn(1);
			}
		}

		// Once a loop breaks, it means that game has come to an end
		if (view.askForResponse("Would you like to play again? Y/N: ") == 'Y') {
			model.resetGame();
			startSession();
		} else {
			System.out.println("Thank you for playing, see you later!");
		}
	}
}
