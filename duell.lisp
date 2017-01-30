;;;;;;************************************************************
;     * Name:  Bryan Leier                                       *
;     * Project:  Duell Implementation 2: LISP                   *
;     * Class:  CMPS 366 - Organization of Programming Langauges *
;     * Date:  October 25, 2016                                  *
;     ************************************************************

; -----------------Game Stuff-------------------

;***************************************************************************
; Function Name: duell
; Purpose: To start up and get into the good stuff (the program itself)
; Parameters: none
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print several messages that indicates what the program is and what number to input for their corresponding actions
; 2. If the user answers 1, start a new game and jump to [function]
; 3. If the user answers 2, load a save game by jumping to [function]
; 4. Otherwise, the user wants to quit. Print a goodbye message and exit
; Assistance received: none
;***************************************************************************

(defun duell ()
  (printMessage "Duell: The Game of Champions!!!")
  (printMessage "Enter the number that corresponds with what you want to do:")
  (printMessage "1: Start a new game. 2: Load a saved game. 3: Quit")
  (let ((answer (askForInput "Enter your decision: " 1 3)))
  (cond ((equal answer 1)
         (startNewRound 0 0))
        ((equal answer 2)
         (resumeGame(loadFile)))
        (t
         (printMessage "Hope to see you again soon... goodbye!")))))

;***************************************************************************
; Function Name: startNewRound
; Purpose: To help start a new round of Duell
; Parameters: computerScore and humanScore, the scores of each player
; Return value: T
; Local variables: firstPlayerResult, the first player determined by the die toss
; Algorithm:
; 1. Display the game components and then play the round with the player who won the die toss
; Assistance received: none
;***************************************************************************

(defun startNewRound (computerScore humanScore)
  (let ((firstPlayerResult (determineFirstPlayer)))
    (displayGameComponents (setUpDice) computerScore humanScore firstPlayerResult)
    (playRound (setUpDice) computerScore humanScore firstPlayerResult)))

;***************************************************************************
; Function Name: startingRoll
; Purpose: To generate the random die roll used to determine who goes first in the game
; Parameters: none
; Return value: a number between 1 and 6
; Local variables: none
; Algorithm:
; 1. Call random on 6 to get a number between 0-5, then add 1 to get a number between 1-6
; Assistance received: none
;***************************************************************************

(defun startingRoll ()
  (+ (random 6) 1))

;***************************************************************************
; Function Name: determineFirstPlayer
; Purpose: To determine who goes first in the round
; Parameters: none
; Return value: a letter of the player type that goes first
; Local variables: humanRoll, the result of the human's die throw, and computerRoll, the result of the computer's die throw
; Algorithm:
; 1. Calculate die rolls for each player
; 2. Print that the round begins with a die toss and the player with the highest goes first
; 3. Print what each player rolled
; 4. If there was a tie, do it again
; 5. If the human rolled greater than the computer, the human goes first. Return H
; 6. Otherwise the computer goes first. Return C
; Assistance received: none
;***************************************************************************

(defun determineFirstPlayer ()
  (let ((humanRoll (startingRoll))
        (computerRoll (startingRoll)))
  (printMessage "The round will begin with a die toss. The player who has the highest number will go first.")
  (princ "The Human rolled a ")
  (princ humanRoll)
  (princ " and the Computer rolled a ")
  (princ computerRoll)
  (printMessage ".")
  (cond ((equal humanRoll computerRoll)
         (printMessage "A tie! A die toss must occur again.")
         (determineFirstPlayer))
        ((> humanRoll computerRoll)
         (printMessage "The Human goes first.")
         'H)
        ((< humanRoll computerRoll)
         (printMessage "The Computer goes first.")
         'C))))

;***************************************************************************
; Function Name: displayGameComponents
; Purpose: To display the various components of the game that need to be displayed
; Parameters: boardModel, the board model to display, computerScore, the computer's score in the tournament, humanScore, the human's score in
; the tournament, and player, the player that is making a move
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print the board by using displayColumnNumbers to show the column numbers, and displayBoardModel for the row numbers and the actual board itself
; 2. Print the amount of wins each player has
; 3. Print the player who is about to make a move on the board
; Assistance received: none
;***************************************************************************

(defun displayGameComponents (boardModel computerScore humanScore player)
  (printMessage "Board:")
  (terpri)
  (princ "     ")
  (displayColumnNumbers 1)
  (terpri)
  (displayBoard boardModel 1)
  (terpri)
  (princ "Computer wins: ")
  (printMessage computerScore)
  (princ "Human wins: ")
  (printMessage humanScore)
  (princ "Next player: ")
  (cond ((equal player 'H)
         (printMessage 'Human))
        (t
         (printMessage 'Computer))))

;***************************************************************************
; Function Name: playRound
; Purpose: To play the game and keep doing turns
; Parameters: boardModel, the boardModel being played on, humanScore, the human's amount of wins, computerScore, the computer's amount of wins,
; and nextPlayer, the player currently making a move on the board
; Return value: T
; Local variables: newBoardModel, the result of the player making a move on the board
; Algorithm:
; 1. Use determineWhoPlays to find whose turn it is then let that player make a move
; 2. Display the game components after the move has been made
; 3. Check for win conditions. If there are, go to calculateWinner to find the winner
; 4. Otherwise, ask if the user wants to save and quit
; 5. If they do, call saveGame and then quit
; 6. Otherwise, recurse into playRound but with newBoardModel passed into the parameters and the opposing player to who just went
; Assistance received: none
;***************************************************************************

(defun playRound (boardModel computerScore humanScore nextPlayer)
  (let ((newBoardModel (determineWhoPlays boardModel nextPlayer)))
    (terpri)
    (cond ((equal nextPlayer 'H)
           (displayGameComponents newBoardModel computerScore humanScore 'C))
          (t
           (displayGameComponents newBoardModel computerScore humanScore 'H)))
    (terpri)
    (cond ((> (checkWinCondition newBoardModel) 0)
           (calculateWinner (checkWinCondition newBoardModel) computerScore humanScore)
           )
          (t
           (cond ((= (askForInput "Would you like to save and quit? Enter 1 to save and quit or 2 to keep playing: " 1 2) 1)
                  (cond ((equal nextPlayer 'H)
                         (saveGame newBoardModel computerScore humanScore 'C))
                        (t
                         (saveGame newBoardModel computerScore humanScore 'H))))
                 (t
                  (cond ((equal nextPlayer 'H)
                         (playRound newBoardModel computerScore humanScore 'C))
                        (t
                         (playRound newBoardModel computerScore humanScore 'H)))))))))
    
  
;***************************************************************************
; Function Name: determineWhoPlays
; Purpose: To call the play function of the next player in the game
; Parameters: boardModel, the board model being played on, and player, the player making a move on the board
; Return value: a board model with the play made on the board
; Local variables: none
; Algorithm:
; 1. If the player is human (H), call humanPlay. Otherwise, call computerPlay
; Assistance received: none
;***************************************************************************

(defun determineWhoPlays (boardModel player)
  (cond ((equal player 'H)
         (humanPlay boardModel))
        (t
         (computerPlay boardModel))))

;***************************************************************************
; Function Name: checkWinCondition
; Purpose: To determine if there is a win condition fulfilled on the board model
; Parameters: boardModel, the board model to check for a win condition on
; Return value: an integer signifying the type of win condition found or none at all
; Local variables: playerKeyDieFound and computerKeyDieFound, booleans that determine if the respective player's key die was found or not
; Algorithm:
; 1. Compute findKeyDie for human and computer
; 2. If player's key die was not found, the human loses. Return 1
; 3. If computer's key die was no found, the computer loses. Return 2
; Otherwise, check the key spaces
; 4. If there is a die on the human's key space and it's of the computer type, the human loses. Return 3
; 5. If there is a die on the computer's key space and it's of the human type, the computer loses. Return 4
; 6. Otherwise, there is no win condition. Return 0
;***************************************************************************


(defun checkWinCondition (boardModel)
  (let* ((playerKeyDieFound (findKeyDie boardModel 'H 8 1))
         (computerKeyDieFound (findKeyDie boardModel 'C 8 1)))
    (cond ((not playerKeyDieFound)
           1)
          ((not computerKeyDieFound)
           2)
          (t
           (cond ((and (isDieOn boardModel 1 5) (isDieOnSpacePlayerType boardModel 1 5 'C))
                  3)
                 ((and (isDieOn boardModel 8 5) (isDieOnSpacePlayerType boardModel 8 5 'H))
                  4)
                 (t
                  0))))))

;***************************************************************************
; Function Name: calculateWinner
; Purpose: To calculate the winner of the round based on the win condition number
; Parameters: winConditionNumber, the number that determines who won and how, and computerScore and humanScore, the scores of each player
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print the winner of the round and ask if they want to play again for every possible win condition scenario
;***************************************************************************

(defun calculateWinner (winConditionNumber computerScore humanScore)
  (cond ((= winConditionNumber 1)
         (printMessage "The Human's key die has been captured. The Computer wins!")
         (askToPlayAgain (+ computerScore 1) humanScore)
         )
        ((= winConditionNumber 2)
         (printMessage "The Computer's key die has been captured. The Human wins!")
         (askToPlayAgain computerScore (+ humanScore 1))
         )
        ((= winConditionNumber 3)
         (printMessage "The Human's key space has been captured. The Computer wins!")
         (askToPlayAgain (+ computerScore 1) humanScore)
         )
        ((= winConditionNumber 4)
         (printMessage "The Computer's key space has been captured. The Human wins!")
         (askToPlayAgain computerScore (+ humanScore 1))
         )
        (t
         )))

;***************************************************************************
; Function Name: askToPlayAgain
; Purpose: To ask the player if they want to play another round of the tournament
; Parameters: computerScore and humanScore, the scores of each player
; Return value: T
; Local variables: userInput, the user's answer to the question
; Algorithm:
; 1. Ask the user if they want to play again
; 2. If they pressed 1, they want to play again. Start a new round with the scores
; 3. Otherwise, they want to quit. Announce the winner
; 4. If somewhere this did not get 1 or 2, just return NIL
;***************************************************************************

(defun askToPlayAgain (computerScore humanScore)
  (let ((userInput (askForInput "Would you like to play again? Enter 1 to start a new round or 2 to quit: " 1 2)))
    (cond ((= userInput 1)
           (startNewRound computerScore humanScore))
          ((= userInput 2)
           (announceWinner computerScore humanScore)
           )
          (t
           NIL))))

;***************************************************************************
; Function Name: announceWinner
; Purpose: To announce the winner of the tournament
; Parameters: computerScore and humanScore, the scores of each player
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print the amount of wins for each player
; 2. Determine who has more wins and print that player. If both players have the same amount of wins, announce a draw
; Assistance received: none
;***************************************************************************

(defun announceWinner (computerScore humanScore)
  (terpri)
  (printMessage "The final results of this tournament are:")
  (terpri)
  (princ "Computer Wins: ")
  (printMessage computerScore)
  (princ "Human Wins: ")
  (printMessage humanScore)
  (terpri)
  (cond ((> humanScore computerScore)
         (printMessage "The Human is this tournament's winner! Congratulations!"))
        ((< humanScore computerScore)
         (printMessage "The Computer is this tournament's winner! Congratulations!"))
        (t
         (printMessage "Both players have the same amount of wins. The tournament ends in a draw!"))))
  

; -----------------Serialization Stuff---------------;

;***************************************************************************
; Function Name: loadFile
; Purpose: To load the text file that has a game saved on it
; Parameters: none
; Return value: the data in the file as an s-expression
; Local variables: filePath, the location of the file to read from
; Algorithm:
; 1. Read from the file and return it as an s-expression
; Assistance received: none
;***************************************************************************

(defun loadFile ()
  (let* ((filePath (open "C:/acl100express/textToLoad.txt"))
         (data (read filePath)))
    (close filePath)
    data))

;***************************************************************************
; Function Name: resumeGame
; Purpose: To resume a game saved from a text file
; Parameters: dataBlock, the s-expression read from the file
; Return value: T
; Local variables: fileBoard, the first element of the dataBlock containing the board display, compWins and humanWins, the next elements in the data block,
; nextPlayer, the last element in the dataBlock, and loadedBoard, a board model with the fileBoard loaded onto it
; Algorithm:
; 1. If the dataBlock is somehow not received, print a message saying so
; 2. Otherwise, initialize all the local variables and get the board model to resume playing on
; 3. Display the game components (board display, scores, next player)
; 4. Go to play the round
; Assistance received: none
;***************************************************************************

(defun resumeGame (dataBlock)
  (cond ((not dataBlock)
         (printMessage "Error reading the file."))
        (t
         (let* ((fileBoard (first dataBlock))
                (compWins (first (rest dataBlock)))
                (humanWins (first (rest (rest dataBlock))))
                (nextPlayer (first (rest (rest (rest dataBlock)))))
                (loadedBoard (restoreBoard fileBoard 1 1 (initializeBoardModel) 8 1)))
           (displayGameComponents loadedBoard compWins humanWins (getNextPlayer nextPlayer))
           (playRound loadedBoard compWins humanWins (getNextPlayer nextPlayer))))))

;***************************************************************************
; Function Name: restoreBoard
; Purpose: To create a board model that represents the one loaded from a text file
; Parameters: dataBlock, the board display read from the file, dataRowNumber and dataColumnNumber, the coordinates of the dataBlock currently
; being visited, boardModel, the board model to modify, and boardRowNumber and boardColumnNumber, the coordinates of the boardModel currently
; being visited
; Return type: a list of lists (board model) containing the board that was restored
; Local variables: currentList, the element in the dataBlock being examined, playerType, the playerType of the die, topNum and rightNum, the top
; and right numbers of the die, dieToPlace, the die generated from getDieToplace, and newBoardModel, the board model that contains the die placed
; on it
; Algorithm:
; 1. Check if dataRowNumber is 9
;    2. If so, we are done searching. Return the board model
; 3. Otherwise, continue:
; 4. Initialize currentList as what is currently being inspected in the dataBlock
; 5. If currentList is not a list, it is an empty space. Skip it and recurse back into the function:
;    6. If dataColumnNumber is less than 9, recurse back into the function with data and board columnNumber incremented
;    7. If dataColumnNumber is 9, recurse back into the function with dataRowNumber incremented and boardRowNumber decremented, and each columnNumber at 1
;    8. Otherwise, something went wrong. Just return the boardModel
; 9. Otherwise, initialize playerType, topNum, rightNum, dieToPlace, and the newBoardModel
; 10. Recurse back into the function with newBoardModel and repeating steps 6-8 for recursion
;***************************************************************************

(defun restoreBoard (dataBlock dataRowNumber dataColumnNumber boardModel boardRowNumber boardColumnNumber)
  (cond ((= dataRowNumber 9)
         boardModel)
        (t
         (let ((currentList (goToColumn (goToRow dataBlock dataRowNumber) dataColumnNumber)))
           (cond ((not (listp currentList))
                  ;; Don't do anything, recurse
                  (cond ((< dataColumnNumber 9)
                         (restoreBoard dataBlock dataRowNumber (+ dataColumnNumber 1) boardModel boardRowNumber (+ boardColumnNumber 1)))
                        ((= dataColumnNumber 9)
                         (restoreBoard dataBlock (+ dataRowNumber 1) 1 boardModel (- boardRowNumber 1) 1))
                        (t
                         boardModel)))
                 (t
                  ;; Place a die
                  (let* ((playerType (first currentList))
                         (topNum (first (rest currentList)))
                         (rightNum (first (rest (rest currentList))))
                         (dieToPlace (getDieToPlace topNum rightNum playerType))
                         (newBoardModel (insertRow boardModel (placeDie dieToPlace boardModel boardRowNumber boardColumnNumber) boardRowNumber)))
                    (cond ((< dataColumnNumber 9)
                           (restoreBoard dataBlock dataRowNumber (+ dataColumnNumber 1) newBoardModel boardRowNumber (+ boardColumnNumber 1)))
                          ((= dataColumnNumber 9)
                           (restoreBoard dataBlock (+ dataRowNumber 1) 1 newBoardModel (- boardRowNumber 1) 1))
                          (t
                           newBoardModel)))))))))

;***************************************************************************
; Function Name: getDieToPlace
; Purpose: To create a die to place on the board by using its top and right numbers
; Parameters: topNum and rightNum, the top and right numbers of the die, and playerType, the player type of the die
; Return value: a list representing the die to place on the board
; Local variables: none
; Algorithm:
; 1. Determine if the die is a key die or not
; 2. If so, just create the key die and place it on the board
; 3. Otherwise call rotateTopNum and then rotateToRightNum on a default die generated inside the function, varying which number to rotate right to
; depending on the player type
; 4. Return the die
; Assistance received: none
;***************************************************************************

(defun getDieToPlace (topNum rightNum playerType)
  (cond ((and (= topNum 1) (= rightNum 1))
         (cond ((equal playerType 'H)
                '(1 1 1 1 1 1 H))
               (t
                '(1 1 1 1 1 1 C))))
        (t
         (cond ((equal playerType 'H)
                (rotateToRightNum (rotateToTopNum '(1 5 3 6 2 4 H) topNum) rightNum))
               (t
                (rotateToRightNum (rotateToTopNum '(1 5 3 6 2 4 C) topNum) (- 7 rightNum)))))))

;***************************************************************************
; Function Name: getNextPlayer
; Purpose: To return a character representing the next player
; Parameters: player, the player going next
; Return value: a character with the player type of the player going next
; Local variables: none
; Algorithm:
; 1. If player is human, return H. Otherwise, return C
; Assistance received: none
;***************************************************************************

(defun getNextPlayer (player)
  (cond ((equal player 'human)
         'H)
        (t
         'C)))

;***************************************************************************
; Function Name: saveGame
; Purpose: To save the game currently being played to a file
; Parameters: boardModel, the board model to save, compWins and humanWins, the wins for each player, and nextPlayer, the player who will
; go next when the game is resumed
; Return value: T
; Local variables: none
; Algorithm:
; 1. Output the proper parenthesis format to the file
; 2. Call convertBoardToSave to convert the board to the appropriate format for the file
; 3. Output compWins, humanWins, and the next player
; 4. Output the proper parentehsis format to close up the file so it can be read into a data block when it is loaded
; Assistance received: none
;***************************************************************************

(defun saveGame (boardModel compWins humanWins nextPlayer)
  (with-open-file (filePath "C:/acl100express/outputFile.txt" :direction :output :if-exists :supersede)
    (format filePath "(~%")
    (format filePath "   (~%")
    (convertBoardToSave boardModel 8 1 filePath)
    (format filePath "  )~&")
    (format filePath "~%")
    (format filePath "  ")
    (format filePath "~a" compWins)
    (format filePath "~%")
    (format filePath "~%")
    (format filePath "  ")
    (format filePath "~a" humanWins)
    (format filePath "~%")
    (format filePath "~%")
    (format filePath "  ")
    (cond ((equal nextPlayer 'H)
           (format filepath "Human~%"))
          (t
           (format filePath "Computer~%")))
    (format filePath ")"))
  (printMessage "The file was successfully saved as outputFile.txt. Until next time!"))

;***************************************************************************
; Function Name: convertBoardToSave
; Purpose: To convert a board model to be able to be written to a text file
; Parameters: boardModel, the board model to convert, rowNumber and columnNumber, the coordinates being visited, and filePath, the text file being
; written to
; Return value: T
; Local variables: none
; Algorithm:
; 1. If the rowNumber is 0, traversal is done. Return
; 2. Otherwise, if the columnNumber is 1, it's a new row. Output an open parenthesis
; 3. If the die on the space is 0, output 0 to the file
; 4. If not, get the die name and output it to the file
; 5. Recurse back into the function appropriately
; Assistance received: none
;***************************************************************************

(defun convertBoardToSave (boardModel rowNumber columnNumber filePath)
  (cond ((= rowNumber 0)
         )
        (t
         (cond ((= columnNumber 1)
                (format filePath "     (")))
         (cond ((= (first (getDieOnSpace boardModel rowNumber columnNumber)) 0)
                (format filePath "0   "))
                (t
                 (format filePath "~a" (getNameOfDieOnSpace boardModel rowNumber columnNumber))
                 (format filePath "  ")))
         (cond ((< columnNumber 9)
                (convertBoardToSave boardModel rowNumber (+ columnNumber 1) filePath))
               ((= columnNumber 9)
                (format filePath ")~%")
                (convertBoardToSave boardModel (- rowNumber 1) 1 filePath))
               (t
                )))))
  

; -----------------Die Stuff-------------------

;***************************************************************************
; Function Name: getTopNum
; Purpose: To get the top number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the top number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getTopNum (die)
  (first die))

;***************************************************************************
; Function Name: getRightNum
; Purpose: To get the right number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the right number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getRightNum (die)
  (first (rest die)))

;***************************************************************************
; Function Name: getFrontNum
; Purpose: To get the front number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the front number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getFrontNum (die)
  (first (rest (rest die))))

;***************************************************************************
; Function Name: getBottomNum
; Purpose: To get the bottom number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the bottom number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getBottomNum (die)
  (first (rest (rest (rest die)))))

;***************************************************************************
; Function Name: getLeftNum
; Purpose: To get the left number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the left number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getLeftNum (die)
  (first (rest (rest (rest (rest die))))))
 
;***************************************************************************
; Function Name: getAwayNum
; Purpose: To get the away number of the die list
; Parameters: a list die to get the number from
; Return value: a number representing the away number on the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the side of the die
; Assistance received: none
;***************************************************************************

(defun getAwayNum (die)
  (first (rest (rest (rest (rest (rest die)))))))

;***************************************************************************
; Function Name: getPlayerType
; Purpose: To get the player type of the die list
; Parameters: a list die to get the player type from
; Return value: a character representing the player type of the die
; Local variables: none
; Algorithm:
; 1. Extract the element in the list that corresponds to the player type of the die
; Assistance received: none
;***************************************************************************

(defun getPlayerType (die)
  (first (rest (rest (rest (rest (rest (rest die))))))))

;***************************************************************************
; Function Name: rollUp
; Purpose: To rearrange the elements in the list so that it looks like the die has rolled up
; Parameters: a list die to rearrange
; Return value: a list that represents the updated die
; Local variables: topNum, rightNum, frontNum, bottomNum, leftNum, and awayNum, numbers that represent the sides on a die,
; and playerType, the player that "owns" the die
; Algorithm:
; 1. Extract each number from the list according to how the list is arranged for dice (top, right, front, bottom, left, away, playerType)
; 2. Return a list that properly represents how the die looks after it is "rolled" up
; Assistance received: none
;***************************************************************************

(defun rollUp (die)
  (let ((topNum (getTopNum die))
        (rightNum (getRightNum die))
        (frontNum (getFrontNum die))
        (bottomNum (getBottomNum die))
        (leftNum (getLeftNum die))
        (awayNum (getAwayNum die))
        (playerType (getPlayerType die)))
  (list frontNum rightNum bottomNum awayNum leftNum topNum playerType)))

;***************************************************************************
; Function Name: rollDown
; Purpose: To rearrange the elements in the list so that it looks like the die has rolled down
; Parameters: a list die to rearrange
; Return value: a list that represents the updated die
; Local variables: topNum, rightNum, frontNum, bottomNum, leftNum, and awayNum, numbers that represent the sides on a die,
; and playerType, the player that "owns" the die
; Algorithm:
; 1. Extract each number from the list according to how the list is arranged for dice (top, right, front, bottom, left, away, playerType)
; 2. Return a list that properly represents how the die looks after it is "rolled" down
; Assistance received: none
;***************************************************************************

(defun rollDown (die)
  (let ((topNum (getTopNum die))
        (rightNum (getRightNum die))
        (frontNum (getFrontNum die))
        (bottomNum (getBottomNum die))
        (leftNum (getLeftNum die))
        (awayNum (getAwayNum die))
        (playerType (getPlayerType die)))
    (list awayNum rightNum topNum frontNum leftNum bottomNum playerType)))

;***************************************************************************
; Function Name: rollLeft
; Purpose: To rearrange the elements in the list so that it looks like the die has rolled left
; Parameters: a list die to rearrange
; Return value: a list that represents the updated die
; Local variables: topNum, rightNum, frontNum, bottomNum, leftNum, and awayNum, numbers that represent the sides on a die,
; and playerType, the player that "owns" the die
; Algorithm:
; 1. Extract each number from the list according to how the list is arranged for dice (top, right, front, bottom, left, away, playerType)
; 2. Return a list that properly represents how the die looks after it is "rolled" left
; Assistance received: none
;***************************************************************************

(defun rollLeft (die)
  (let ((topNum (getTopNum die))
        (rightNum (getRightNum die))
        (frontNum (getFrontNum die))
        (bottomNum (getBottomNum die))
        (leftNum (getLeftNum die))
        (awayNum (getAwayNum die))
        (playerType (getPlayerType die)))
    (list rightNum bottomNum frontNum leftNum topNum awayNum playerType)))

;***************************************************************************
; Function Name: rollRight
; Purpose: To rearrange the elements in the list so that it looks like the die has rolled right
; Parameters: a list die to rearrange
; Return value: a list that represents the updated die
; Local variables: topNum, rightNum, frontNum, bottomNum, leftNum, and awayNum, numbers that represent the sides on a die,
; and playerType, the player that "owns" the die
; Algorithm:
; 1. Extract each number from the list according to how the list is arranged for dice (top, right, front, bottom, left, away, playerType)
; 2. Return a list that properly represents how the die looks after it is "rolled" right
; Assistance received: none
;***************************************************************************

(defun rollRight (die)
  (let ((topNum (getTopNum die))
        (rightNum (getRightNum die))
        (frontNum (getFrontNum die))
        (bottomNum (getBottomNum die))
        (leftNum (getLeftNum die))
        (awayNum (getAwayNum die))
        (playerType (getPlayerType die)))
    (list leftNum topNum frontNum rightNum bottomNum awayNum playerType)))

;***************************************************************************
; Function Name: rotateToTopNum
; Purpose: To rotate the die so that the number in the parameter is on the top of the die
; Parameters: a list die to rotate and a number topInput to rotate to
; Return value: a list that represents the updated die
; Local variables: None
; Algorithm: 
; 1. Determine if the number on the top of the die matches the topInput
; 2. If not, call certain functions until it is
; Assistance received: none
;***************************************************************************

(defun rotateToTopNum (die topInput)
  (cond ((equal topInput 1)
          die             )
        ((equal topInput 2)
         (rollRight die)  )
        ((equal topInput 3)
         (rollUp die)     )
        ((equal topInput 4)
         (rollDown die)   )
        ((equal topInput 5)
         (rollLeft die)   )
        ((equal topInput 6)
           (rollUp die)      
           (rollUp die))))

;***************************************************************************
; Function Name: rotateLeft
; Purpose: To rearrange the elements in the list so that it looks like the die has been rotated to the left
; Parameters: a list die to rearrange
; Return value: a list that represents the updated die
; Local variables: topNum, rightNum, frontNum, bottomNum, leftNum, and awayNum, numbers that represent the sides on a die
; Algorithm:
; 1. Extract each number from the list according to how the list is arranged for dice (top, right, front, bottom, left, away, playerType)
; 2. Return a list that properly represents how the die looks after it is "rotated" left
; Assistance received: none
;***************************************************************************

(defun rotateLeft (die)
  (let ((topNum (getTopNum die))
        (rightNum (getRightNum die))
        (frontNum (getFrontNum die))
        (bottomNum (getBottomNum die))
        (leftNum (getLeftNum die))
        (awayNum (getAwayNum die))
        (playerType (getPlayerType die)))
    (list topNum awayNum rightNum bottomNum frontNum leftNum playerType)))

;***************************************************************************
; Function Name: rotateToRightNum
; Purpose: To rotate the die to the left until the number in the parameter is on the right side of the die
; Parameters: a list die to rotate and a number rightInput to rotate to
; Return value: a list that represents the updated die
; Local Variables: rightNum, a number that contains the number on the right of the die
; Algorithm:
; 1. Determine if the number on the right of the die matches rightInput
; 2. If not, call the function again but with rotateLeft applied to the die
; Assistance received: none
;***************************************************************************

(defun rotateToRightNum (die rightInput)
  (let ((rightNum (getRightNum die)))
    (cond ((equal rightNum rightInput)
            die                      )
          (t
          (rotateToRightNum (rotateLeft die) rightInput)))))

;***************************************************************************
; Function Name: displayDieName
; Purpose: To display the name of the die
; Parameters: a list die to display the name of
; Return value: a list that represents the name of the die
; Local variables: topNum and rightNum, numbers to display in the dieName, and playerType, a letter that shows which player "owns" the die
; Algorithm:
; 1. Extract the first two numbers from the list according to how it is arranged for dice
; 2. Extract the last element of the list, the playerType
; 3. Return a list that represents the die name
; Assistance received: none
;***************************************************************************

(defun displayDieName (die)
  (let ((topNum (getTopNum die))
        (leftNum (getLeftNum die))
        (rightNum (getRightNum die))
        (playerType (getPlayerType die)))
  (cond ((equal playerType 'C)
         (list playerType topNum leftNum))
         (t
         (list playerType topNum rightNum)))))

;***************************************************************************
; Function Name: rollDie
; Purpose: To roll the die in the direction specified in the parameter
; Parameters: a list die to roll and a string direction to roll the die in
; Return value: a list that represents the updated die
; Local variables: none
; Algorithm:
; 1. Look at what direction the direction parameter is telling the die to roll in
; 2. Call the respective function to roll the die in that direction
; Assistance received: none
;***************************************************************************

(defun rollDie (die direction)
  (cond ((equal direction "up")
         (rollUp die))
        ((equal direction "down")
         (rollDown die))
        ((equal direction "left")
         (rollLeft die))
        ((equal direction "right")
         (rollRight die))))

;***************************************************************************
; Function Name: isKeyDie
; Purpose: To determine if the die is a key die or not
; Parameters: a list die to check the sides of
; Return value: a boolean value that determines if the die is a key die or not
; Local variables: none
; Algorithm:
; 1. Check if the top and right numbers on the die are equal to 1. If they are, this is a key die. Return true
; 2. Otherwise, it isn't. Return false
; Assistance received: none
;***************************************************************************

(defun isKeyDie (die)
  (cond ((and (equal (getTopNum die) 1) (equal (getRightNum die) 1))
          t)
        (t
         NIL)))

;***************************************************************************
; Function Name: isDiePlayerType
; Purpose: To determine if the die is of the player type in the parameter or not
; Parameters: a list die to check the player type of and playerTypeCheck, the player type to check for
; Return value: a boolean value that determines if the die is of playerTypeCheck or not
; Local variables: playerType, the player type of the die
; Algorithm:
; 1. Initialize playerType as the player type of the die
; 2. If playerType and playerTypeCheck are equal, it is of the player type that we're looking for. Return true
; 3. Otherwise, it isn't. Return false
; Assistance received: none
;***************************************************************************

(defun isDiePlayerType (die playerTypeCheck)
  (let ((playerType (getPlayerType die)))
  (cond ((equal playerType playerTypeCheck)
         t)
        (t
         NIL))))

;-------------Board Stuff----------------

;***************************************************************************
; Function Name: displayRow
; Purpose: To return a list representing the row passed into the function
; Parameters: row, a list containing information about the row on the board
; Return value: a list that represents the row
; Local variables: none
; Algorithm:
; 1. Look at the row passed into the function
; 2. If it's null, return an empty list
; 3. If the first element is equal to 0, cons 0 to the rest of the display
; 4. Otherwise, cons the name of the die to the rest of the display
; Assistance received: none
;***************************************************************************

(defun displayRow (row)
  (cond ((null row)
                  )
        ((equal (first (first row)) 0)
         (princ 0)
         (princ "   ")
         (displayRow (rest row)))
        (t
         (princ (displayDieName (first row)))
         (princ "  ")
         (displayRow (rest row)))))

;***************************************************************************
; Function Name: displayColumnNumbers
; Purpose: To display the column numbers that correspond to the board display
; Parameters: number, the number to print that represents the column number
; Return value: T
; Local variables: none
; Algorithm:
; 1. Check if the number in the function is less than 10
; 2. If so, print it, some spaces, and then recursively call the function again with the number incremented by one
; 3. Otherwise, do nothing
; Assistance received: none
;***************************************************************************

(defun displayColumnNumbers (number)
  (cond ((< number 10)
         (princ number)
         (princ "   ")
         (displayColumnNumbers (+ number 1)))
        (t
          )))

;***************************************************************************
; Function Name: displayBoard
; Purpose: To display the board with the row lists passed into the function
; Parameters: boardModel, a list containing rows on the board, and number, the number to initialize the row count by
; Return value: T
; Local variables: none
; Algorithm:
; 1. Look at the boardModel passed into the function
; 2. If it's null, print nothing
; 3. Otherwise, use a combination of princ and printMessage to display the row numbers and the rows themselves
; Assistance received: none
;***************************************************************************

(defun displayBoard (boardModel number)
  (cond ((null boardModel)
                         )
        (t
        (displayBoard (rest boardModel) (+ number 1))
        (princ number)
        (princ "   ")
         (displayRow (first boardModel))
         (terpri))))

;***************************************************************************
; Function Name: initializeBoardModel
; Purpose: To initialize a list of lists that will represent the board model
; Parameters: none
; Return value: a list containing lists of lists of 0
; Local variables: none
; Algorithm:
; 1. Return a list of lists containing "empty spaces" (lists containing 0)
; Assistance received: none
;***************************************************************************

(defun initializeBoardModel ()
  '(((0) (0) (0) (0) (0) (0) (0) (0) (0)) ((0) (0) (0) (0) (0) (0) (0) (0) (0)) ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0)) ((0) (0) (0) (0) (0) (0) (0) (0) (0)) ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0)) ((0) (0) (0) (0) (0) (0) (0) (0) (0))))

;***************************************************************************
; Function Name: goToRow
; Purpose: To go to the row specified in number in the boardModel (the lists)
; Parameters: boardModel, a list of lists that represents the board model, and number, the row number that will be returned
; Return value: a list that represents the row
; Local variables: none
; Algorithm:
; 1. If the number passed into the function is 1, return the first list in boardModel
; 2. Otherwise, search the next row and decrement number by 1
; Assistance received: none
;***************************************************************************

(defun goToRow (boardModel number)
  (cond ((= number 1)
         (first boardModel))
        (t
        (goToRow (rest boardModel) (- number 1)))))

;***************************************************************************
; Function Name: goToColumn
; Purpose: To go to the column specified in number in the row (list)
; Parameters: row, the list that represents the row, and number, the column number that will be returned
; Return value: a list that represents what is currently occupying the space
; Local variables: none
; Algorithm:
; 1. If the number passed into the function is 1, return the first element in row
; 2. Otherwise, search the next column and decrement number by 1
; Assistance received: none
;***************************************************************************

(defun goToColumn (row number)
  (cond ((= number 1)
         (first row))
         (t
         (goToColumn (rest row) (- number 1)))))

;***************************************************************************
; Function Name: getSpacesBefore
; Purpose: To create a list of the spaces before a given column position in a row (list)
; Parameters: row, the list that represents the row, and number, the column number to stop before
; Return value: a list with the rows before the space in the parameters
; Local variables: none
; Algorithm:
; 1. If number is equal to 1, return an empty list
; 2. Otherwise, cons the first element of row and the recursion of the rest of row and decrement number by 1
; Assistance received: none
;***************************************************************************

(defun getSpacesBefore (row number)
  (cond ((= number 1)
         ()         )
        (t
        (cons (first row) (getSpacesBefore (rest row) (- number 1))))))

;***************************************************************************
; Function Name: getSpacesAfter
; Purpose: To create a list of the spaces after a given column position in a row (list)
; Parameters: row, the list that represents the row, and number, the column number to begin after
; Return value: a list with the rows after the space in the parameters
; Local variables: none
; Algorithm:
; 1. If the number is 1, return the rest of the list
; 2. Otherwise, recurse into the rest of the list and decrement number by 1
; Assistance received: none
;***************************************************************************

(defun getSpacesAfter (row number)
  (cond ((= number 1)
         (rest row) )
        (t
        (getSpacesAfter (rest row) (- number 1)))))

;***************************************************************************
; Function Name: placeDie
; Purpose: To place a die on the boardModel at the specified row and column
; Parameters: die, a list to insert into the board model, boardModel, a list of lists representing the board model, and row and column, the row
; and column that the die will be placed on
; Return value: a row with the die placed on it
; Local variables: none
; Algorithm:
; 1. Initialize spacesBefore as the spaces before the row and column position to insert into
; 2. Initialize spacesAfter as the spaces after the row and column position to insert into
; 3. Append spacesBefore with a cons of die and spacesAfter and return that list
; Assistance received: none
;***************************************************************************

(defun placeDie (die boardModel row column)
  (let ((spacesBefore (getSpacesBefore (goToRow boardModel row) column))
        (spacesAfter (getSpacesAfter (goToRow boardModel row) column)))
  (append spacesBefore (cons die spacesAfter))))

;***************************************************************************
; Function Name: getRowsBefore
; Purpose: To create a list of the rows before a given row position in the boardModel (list of lists)
; Parameters: boardModel, the list that represents the rows on the board, and number, the row number to stop before
; Return value: a list of the rows before the row in the parameter
; Local variables: none
; Algorithm:
; 1. If number is equal to 1, return an empty list
; 2. Otherwise, cons the first list in boardModel and the recursion of the rest of boardModel, decrement number by 1
; Assistance received: none
;***************************************************************************

(defun getRowsBefore (boardModel number)
  (cond ((= number 1)
         ()         )
        (t
        (cons (first boardModel) (getRowsBefore (rest boardModel) (- number 1))))))

;***************************************************************************
; Function Name: getRowsAfter
; Purpose: To create a list of the rows after a given row position in the boardModel (list of lists)
; Parameters: boardModel, the list that represents the rows on the board, and number, the row number to begin after
; Return value: a list of the rows after the row in the parameter
; Local variables: none
; Algorithm:
; 1. If the number is 1, return the rest of the list
; 2. Otherwise, recurse into the rest of the list and decrement number by 1
; Assistance received: none
;***************************************************************************

(defun getRowsAfter (boardModel number)
  (cond ((= number 1)
         (rest boardModel))
        (t
        (getRowsAfter (rest boardModel) (- number 1)))))

;***************************************************************************
; Function Name: insertRow
; Purpose: To insert a row into the boardModel at the specified row position and return a new boardModel with this in mind
; Parameters: boardModel, the board model to insert the row into, row, the row to insert into the board model, and number, the row's number
; Return value: a list of lists (board model) with the row inserted into it
; Local variables: rowsBefore, a list of the rows before the row position, and rowsAfter, a list of rows after the row position
; Algorithm:
; 1. Initialize rowsBefore and rowsAfter using their respective functions
; 2. Append rowsBefore with the cons of the row passed into the parameter and rowsAfter
; Assistance received: none
;***************************************************************************

(defun insertRow (boardModel row number)
  (let ((rowsBefore (getRowsBefore boardModel number))
        (rowsAfter (getRowsAfter boardModel number)))
    (append rowsBefore (cons row rowsAfter))))

;***************************************************************************
; Function Name: updateBoardModel
; Purpose: To update the board model and return a new one
; Parameters: boardModel, the board model to update, row1, the first row to insert, row2, the second row to insert, and row1number and row2number,
; the numbers of the respective rows to insert into the model
; Return value: the board model (list of lists) with the rows inserted into it
; Local variables: almostBoardModel, the board model after the first row is inserted into it
; Algorithm:
; 1. Initialize almostBoardModel as a board model with the first row inserted into it
; 2. Insert the second row into almostBoardModel and return that board model
; Assistance received: none
;***************************************************************************

(defun updateBoardModel (boardModel row1 row1number row2 row2number)
  (let ((almostBoardModel (insertRow boardModel row1 row1number)))
  (insertRow almostBoardModel row2 row2number)))

;***************************************************************************
; Function Name: isDieOn
; Purpose: To determine if there is a die on the space specified by row and column
; Parameters: boardModel, the list of lists representing the board, row and column, the row and column to search in the board model
; Return value: a boolean value that determines if a die is occupying the space or not
; Local variables: spaceToSearch, the space that will be searched for a die
; Algorithm:
; 1. Initialize spaceToSearch as the space in the board model, which is received by using a combination of goToColumn and goToRow
; 2. If the first element of the spaceToSearch list is 0, there is nothing on the space. Otherwise, there is something on the space
; Assistance received: none
;***************************************************************************

(defun isDieOn (boardModel row column)
  (let ((spaceToSearch (goToColumn (goToRow boardModel row) column)))
    (cond ((equal (first spaceToSearch) 0)
            NIL                          )
          (t
           t                ))))

;***************************************************************************
; Function Name: moveDie
; Purpose: To move a die at a row and column coordinate to another row and column coordinate in a given direction
; Parameters: boardModel, the list of lists representing the board, origRow and origColumn, the coordinates of where the die is located,
; newRow and newColumn, the coordinates of where the die will be moved to, and direction, the direction in which the die will roll
; Return value: a board model (list of lists) with the die move represented
; Local variables: dieToMove, the die that will be moved on the board
; Algorithm:
; 1. Initialize dieToMove as the die that will be moved, but rolled in the given direction
; 2. Call updateBoardModel to return an updated board model with the new row list containing the new die and the old row list with where the
; die was originally located
; Assistance received: none
;***************************************************************************

(defun moveDie (boardModel origRow origColumn newRow newColumn direction)
  (let ((dieToMove (rollDie (goToColumn (goToRow boardModel origRow) origColumn) direction)))
    (updateBoardModel boardModel (placeDie dieToMove boardModel newRow newColumn) newRow
                      (placeDie '(0) (insertRow boardModel (placeDie dieToMove boardModel newRow newColumn) newRow) origRow origColumn) origRow)))

;***************************************************************************
; Function Name: performRoll
; Purpose: To make a die at a row and column coordinate move in the direction specified and return a board model with this change implemented
; Parameters: boardModel, the list of lists representing the board, row and column, the coordinates of where the die is located,
; and direction, the direction to roll the die in
; Return value: a board model (list of lists) with the die move represented
; Local variables: none
; Algorithm:
; 1. Check the direction specified in the parameter
; 2. Call moveDie and move the die to the appropriate row and column coordinate according to the direction told to move in
; Assistance received: none
;***************************************************************************

(defun performRoll (boardModel row column direction)
  (cond ((equal direction "up")
         (moveDie boardModel row column (+ row 1) column "up"))
        ((equal direction "down")
         (moveDie boardModel row column (- row 1) column "down"))
        ((equal direction "left")
         (moveDie boardModel row column row (- column 1) "left"))
        ((equal direction "right")
         (moveDie boardModel row column row (+ column 1) "right"))))

;***************************************************************************
; Function Name: setUpDice
; Purpose: To set up the dice on the board model for a new game of Duell
; Parameters: none
; Return value: a board model with the starting positions of all the dice needed to play the game
; Local variables: none
; Algorithm:
; 1. Return a board model list of lists with the starting positions of dice in a game in Duell
; Assistance received: none
;***************************************************************************

(defun setUpDice ()
  '(((5 6 3 2 1 4 H) (1 5 3 6 2 4 H) (2 1 3 5 6 4 H) (6 2 3 1 5 4 H) (1 1 1 1 1 1 H) (6 2 3 1 5 4 H) (2 1 3 5 6 4 H) (1 5 3 6 2 4 H) (5 6 3 2 1 4 H))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((0) (0) (0) (0) (0) (0) (0) (0) (0))
    ((5 1 4 2 6 3 C) (1 2 4 6 5 3 C) (2 6 4 5 1 3 C) (6 5 4 1 2 3 C) (1 1 1 1 1 1 C) (6 5 4 1 2 3 C) (2 6 4 5 1 3 C) (1 2 4 6 5 3 C) (5 1 4 2 6 3 C))))

;***************************************************************************
; Function Name: getDieOnSpace
; Purpose: To return the list that represents the die on a given space in the board model
; Parameters: boardModel, the board model to search through, and row and column, the coordinates of the die to look for
; Return value: a list that represents the die on the space
; Local variables: none
; Algorithm:
; 1. Return the result of goToColumn by using the row returned by goToRow
; Assistance received: none
;***************************************************************************

(defun getDieOnSpace (boardModel row column)
  (goToColumn (goToRow boardModel row) column))

;***************************************************************************
; Function Name: getNameOfDieOnSpace
; Purpose: To return the name of the die on a given space in the board model
; Parameters: boardModel, the board model to search through, and row and column, the coordinates of the die to look for
; Return value: a list that represents the name of the die on the space
; Local variables: none
; Algorithm:
; 1. Call displayDieName on the result of getDieOnSpace with the die's coordinates
; Assistance received: none
;***************************************************************************

(defun getNameOfDieOnSpace (boardModel row column)
  (displayDieName (getDieOnSpace boardModel row column)))

;***************************************************************************
; Function Name: isDieOnSpaceKeyDie
; Purpose: To determine if the die on the space given by the coordinates is the key die or not
; Parameters: boardModel, the board model to search through, and row and column, the coordinates of the die to look for
; Return value: a boolean that determines if the die on the space is a key die or not
; Local variables: dieToCheck, the die at the coordinates
; Algorithm:
; 1. Initialize dieToCheck as the die on the space given by the result of getDieOnSpace
; 2. Call isKeyDie on dieToCheck. If it returns true, the die on the space is a key die. Return true
; 3. Otherwise, it isn't. Return false
; Assistance received: none
;***************************************************************************

(defun isDieOnSpaceKeyDie (boardModel row column)
  (let ((dieToCheck (getDieOnSpace boardModel row column)))
  (cond ((isKeyDie dieToCheck)
         t)
        (t
         NIL))))

;***************************************************************************
; Function Name: isDieOnSpacePlayerType
; Purpose: To determine if the die on the space given by the coordinates is the player type passed into the function or not
; Parameters: boardModel, the board model to search through, row and column, the coordinates of the die to look for, and playerType, the player type
; to check on the die
; Return value: a boolean value that determines if the die on the space is of playerType or not
; Local variables: dieToCheck, the die at the coordinates
; Algorithm:
; 1. Initialize dieToCheck as the die on the space given by the result of getDieOnSpace
; 2. Call isDiePlayerType on dieToCheck with playerType. If it returns true, the die on the space is of the player type. Return true
; 3. Otherwise, it isn't. Return false
; Assistance received: none
;***************************************************************************

(defun isDieOnSpacePlayerType (boardModel row column playerType)
  (let ((dieToCheck (getDieOnSpace boardModel row column)))
  (cond ((isDiePlayerType dieToCheck playerType)
         t)
        (t
         NIL))))

;***************************************************************************
; Function Name: getDieOnSpaceTopNum
; Purpose: To get the top number of the die on the space given by the coordinates
; Parameters: boardModel, the board model to search through, and row and column, the coordinates of the die to look for
; Return value: a number that represents the top number of the die on the space
; Local variables: dieToCheck, the die at the coordinates
; Algorithm:
; 1. Initialize dieToCheck as the die on the space given by the result of getDieOnSpace
; 2. Return the result of getTopNum on dieToCheck
; Assistance received: none
;***************************************************************************

(defun getDieOnSpaceTopNum (boardModel row column)
  (let ((dieToCheck (getDieOnSpace boardModel row column)))
    (getTopNum dieToCheck)))

;***************************************************************************
; Function Name: findKeyDie
; Purpose: To determine if the key die of a player type is on the board or not
; Parameters: boardModel, the board model to search, playerType, the player type of the die to look for, and rowNumber and columnNumber, the space currently
; being visited on the boardModel
; Return value: a boolean determining if the die was found or not
; Local variables: none
; Algorithm:
; 1. If the rowNumber is 0, the board was searched and the key die was not found. Return false
; 2. Otherwise, determine if there is a die on (rowNumber,columnNumber), if it's of playerType, and if it's a key die
; 3. If so, it's on the board. Return true
; 4. Otherwise, recurse back into the function:
;    5. If columnNumber is less than 9, recurse back into the function with columnNumber incremented
;    6. If columnNumber is 9, recurse back into the function with rowNumber decremented and columnNumber as 1
;    7. Otherwise, something went wrong somewhere. Return false
;***************************************************************************

(defun findKeyDie (boardModel playerType rowNumber columnNumber)
  (cond ((= rowNumber 0)
         NIL)
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType)
                     (isDieOnSpaceKeyDie boardModel rowNumber columnNumber))
                T)
               (t
                (cond ((< columnNumber 9)
                       (findKeyDie boardModel playerType rowNumber (+ columnNumber 1)))
                      ((= columnNumber 9)
                       (findKeyDie boardModel playerType (- rowNumber 1) 1))
                      (t
                       NIL)))))))

; -----------------Player Stuff-------------------

;***************************************************************************
; Function Name: moveUp
; Purpose: To move a die up to the specified row coordinate on the board model
; Parameters: boardModel, the board model to make a move on, row and column, the row and column to move from, and spaceCoordinate, the row
; the die will move to
; Return value: a board model (list of lists) with the die move implemented
; Local variables: none
; Algorithm:
; 1. Determine if the row in the parameter is less than spaceCoordinate
; 2. If so, call moveUp recursively on the board model that is returned by performRoll moving the die up by one and at the new row
; position of the die
; 3. If it does not need to move up any longer, return the new board model
; Assistance received: none
;***************************************************************************

(defun moveUp (boardModel row column spaceCoordinate)
  (cond ((< row spaceCoordinate)
         (moveUp (performRoll boardModel row column "up") (+ row 1) column spaceCoordinate))
        (t
         boardModel)))

;***************************************************************************
; Function Name: moveDown
; Purpose: To move a die down to the specified row coordinate on the board model
; Parameters: boardModel, the board model to make a move on, row and column, the row and column to move from, and spaceCoordinate, the row
; the die will move to
; Return value: a board model (list of lists) with the die move implemented
; Local variables: none
; Algorithm:
; 1. Determine if the row in the parameter is greater than spaceCoordinate
; 2. If so, call moveDown recursively on the board model that is returned by performRoll moving the die down by one and at the new row
; position of the die
; 3. If it does not need to move down any longer, return the new board model
; Assistance received: none
;***************************************************************************

(defun moveDown (boardModel row column spaceCoordinate)
  (cond ((> row spaceCoordinate)
         (moveDown (performRoll boardModel row column "down") (- row 1) column spaceCoordinate))
        (t
         boardModel)))

;***************************************************************************
; Function Name: moveRight
; Purpose: To move a die right to the specified column coordinate on the board model
; Parameters: boardModel, the board model to make a move on, row and column, the row and column to move from, and spaceCoordinate, the column
; the die will move to
; Return value: a board model (list of lists) with the die move implemented
; Local variables: none
; Algorithm:
; 1. Determine if the column in the parameter is less than spaceCoordinate
; 2. If so, call moveRight recursively on the board model that is returned by performRoll moving the die right by one and at the new column
; position of the die
; 3. If it does not need to move right any longer, return the new board model
; Assistance received: none
;***************************************************************************

(defun moveRight (boardModel row column spaceCoordinate)
  (cond ((< column spaceCoordinate)
         (moveRight (performRoll boardModel row column "right") row (+ column 1) spaceCoordinate))
        (t
         boardModel)))

;***************************************************************************
; Function Name: moveLeft
; Purpose: To move a die left to the specified column coordinate on the board model
; Parameters: boardModel, the board model to make a move on, row and column, the row and column to move from, and spaceCoordinate, the column
; the die will move to
; Return value: a board model (list of lists) with the die move implemented
; Local variables: none
; Algorithm:
; 1. Determine if the column in the parameter is greater than spaceCoordinate
; 2. If so, call moveLeft recursively on the board model that is returned by performRoll moving the die right by one and at the new column
; position of the die
; 3. If it does not need to move left any longer, return the new board model
; Assistance received: none
;***************************************************************************

(defun moveLeft (boardModel row column spaceCoordinate)
  (cond ((> column spaceCoordinate)
         (moveLeft (performRoll boardModel row column "left") row (- column 1) spaceCoordinate))
        (t
         boardModel)))

;***************************************************************************
; Function Name: makeMove
; Purpose: To make the move that the player wants to make on the board model
; Parameters: boardModel, the board model to make a move on, row and column, the row and column to move from, spaceCoordinate, the row or column
; that the die will move to, and direction, the direction the die will move in to get to spaceCoordinate
; Return value: a board model (list of lists) with the die move implemented
; Local variables: none
; Algorithm:
; 1. Determine if the player wants to move frontally
;    2. If so, determine if the spaceCoordinate is greater than row
;       3. If so, move up. Otherwise, move down
; 4. Otherwise, the player wants to move laterally
;    5. Determine if the spaceCoordinate is greater than column
;       6. If so, move right. Otherwise, move left
; Assistance received: none
;***************************************************************************

(defun makeMove (boardModel row column spaceCoordinate direction)
  (cond ((equal direction "frontally")
         (cond ((> spaceCoordinate row)
                (moveUp boardModel row column spaceCoordinate))
                (t
                (moveDown boardModel row column spaceCoordinate))))
         (t
         (cond ((> spaceCoordinate column)
                (moveRight boardModel row column spaceCoordinate))
                (t
                 (moveLeft boardModel row column spaceCoordinate))))))

;***************************************************************************
; Function Name: checkSpacesBetweenColumns
; Purpose: To check the spaces between two column coordinates to see if a move is possible
; Parameters: boardModel, the board model to check if a move is possible on, row and column, the row and column of the die to move, coordToMoveTo, the
; column coordinate to check if the die can be moved to, spacesToMove, the amount of spaces to move, and playerType, the player type of the die
; Return value: a boolean that determines whether or not a move is possible between the spaces
; Local variables: none
; Algorithm:
; 1. Determine if coordToMoveTo is greater than the die's column
;    2. If so, check if there is a die on the space in the column next to the position being checked
;    3. If not, recurse into the function and check the column over the one previously being checked
;       4. If there is a die, check if spacesToMove is 1
;          5. If it isn't, a die cannot be moved regardless of who "owns the die," return false
;          6. If it is, check the playerType of the die occupying the space
;             7. If it is not of the same player type, the die can move there. Return true
;             8. Otherwise, return false
; 9. Determine if coordToMoveTo is less than the die's column
;    10. If so, check if there is a die on the space in the column before the position being checked
;    11. If not, recurse into the function and check the column over the one previously being checked
;        12. If there is a die, check if spacesToMove is 1
;            13. If it isn't, a die cannot be moved regardless of who "owns the die," return false
;            14. If it is, check the playerType of the die occupying the space
;                15. If it is not of the same player type, the die can move there. Return true
;                16. Otherwise, return false
; 17. Otherwise, a move is possible. Return true
; Assistance received: none
;***************************************************************************

(defun checkSpacesBetweenColumns (boardModel row column coordToMoveTo spacesToMove playerType)
  (cond ((> coordToMoveTo column)
         (cond ((not (isDieOn boardModel row (+ column 1)))
                (checkSpacesBetweenColumns boardModel row (+ column 1) coordToMoveTo (- spacesToMove 1) playerType))
               (t
                (cond ((not (= spacesToMove 1))
                       NIL)
                      (t
                       (cond ((not (isDieOnSpacePlayerType boardModel row (+ column 1) playerType))
                              t)
                             (t
                              NIL)))))))
        ((< coordToMoveTo column)
         (cond ((not (isDieOn boardModel row (- column 1)))
                (checkSpacesBetweenColumns boardModel row (- column 1) coordToMoveTo (- spacesToMove 1) playerType))
               (t
                (cond ((not (= spacesToMove 1))
                       NIL)
                      (t
                       (cond ((isDieOnSpacePlayerType boardModel row (- column 1) playerType)
                              NIL)
                             (t
                              t)))))))
        ((= coordToMoveTo column)
         t)))

;***************************************************************************
; Function Name: checkSpacesBetweenRows
; Purpose: To check the spaces between two row coordinates to see if a move is possible
; Parameters: boardModel, the board model to check if a move is possible on, row and column, the row and column of the die to move, coordToMoveTo, the
; row coordinate to check if the die can be moved to, spacesToMove, the amount of spaces to move, and playerType, the player type of the die
; Return value: a boolean that determines whether or not a move is possible between the spaces
; Local variables: none
; Algorithm:
; 1. Determine if coordToMoveTo is greater than the die's row
;    2. If so, check if there is a die on the space in the row next to the position being checked
;    3. If not, recurse into the function and check the row over the one previously being checked
;       4. If there is a die, check if spacesToMove is 1
;          5. If it isn't, a die cannot be moved regardless of who "owns the die," return false
;          6. If it is, check the playerType of the die occupying the space
;             7. If it is not of the same player type, the die can move there. Return true
;             8. Otherwise, return false
; 9. Determine if coordToMoveTo is less than the die's row
;    10. If so, check if there is a die on the space in the row before the position being checked
;    11. If not, recurse into the function and check the row over the one previously being checked
;        12. If there is a die, check if spacesToMove is 1
;            13. If it isn't, a die cannot be moved regardless of who "owns the die," return false
;            14. If it is, check the playerType of the die occupying the space
;                15. If it is not of the same player type, the die can move there. Return true
;                16. Otherwise, return false
; 17. Otherwise, a move is possible. Return true
; Assistance received: none
;***************************************************************************

(defun checkSpacesBetweenRows (boardModel row column coordToMoveTo spacesToMove playerType)
  (cond ((> coordToMoveTo row)
         (cond ((not (isDieOn boardModel (+ row 1) column))
                (checkSpacesBetweenRows boardModel (+ row 1) column coordToMoveTo (- spacesToMove 1) playerType))
               (t
                (cond ((not (= spacesToMove 1))
                       NIL)
                      (t
                       (cond ((not (isDieOnSpacePlayerType boardModel (+ row 1) column playerType))
                              t)
                             (t
                              NIL)))))))
        ((< coordToMoveTo row)
         (cond ((not (isDieOn boardModel (- row 1) column))
                (checkSpacesBetweenRows boardModel (- row 1) column coordToMoveTo (- spacesToMove 1) playerType))
               (t
                (cond ((not (= spacesToMove 1))
                       NIL)
                      (t
                       (cond ((isDieOnSpacePlayerType boardModel (- row 1) column playerType)
                              NIL)
                             (t
                              t)))))))
        ((= coordToMoveTo row)
         t)))

;***************************************************************************
; Function Name: canMoveLaterally
; Purpose: To determine if a die can be moved laterally on the board
; Parameters: boardModel, the board model that a move wants to be made on, row and column, the row and column of the die to move, spaceColumn,
; the column to check if the die can be moved to, spacesToMove, the amount of spaces to move, and playerType, the player type of the die
; Return value: a boolean that determines whether or not a lateral move is possible
; Local variables: none
; Algorithm:
; 1. If spacesToMove is 0, the die cannot move anywhere. Return false
; 2. If the column and spaceColumn are equal, only a frontal move is possible. Return false
; 3. Otherwise, see if checkSpacesBetweenColumns is true
;    4. If so, return true
;    5. Otherwise, return false
; Assistance received: none
;***************************************************************************

(defun canMoveLaterally (boardModel row column spaceColumn spacesToMove playerType)
  (cond ((= spacesToMove 0)
         NIL)
        ((= column spaceColumn)
         NIL)
        (t
         (cond ((checkSpacesBetweenColumns boardModel row column spaceColumn spacesToMove playerType)
                t)
               (t
                NIL)))))

;***************************************************************************
; Function Name: canMoveFrontally
; Purpose: To determine if a die can be moved frontally on the board
; Parameters: boardModel, the board model that a move wants to be made on, row and column, the row and column of the die to move, spaceRow,
; the row to check if the die can be moved to, spacesToMove, the amount of spaces to move, and playerType, the player type of the die
; Return value: a boolean that determines whether or not a lateral move is possible
; Local variables: none
; Algorithm:
; 1. If spacesToMove is 0, the die cannot move anywhere. Return false
; 2. If the row and spaceRow are equal, only a lateral move is possible. Return false
; 3. Otherwise, see if checkSpacesBetweenRows is true
;    4. If so, return true
;    5. Otherwise, return false
; Assistance received: none
;***************************************************************************

(defun canMoveFrontally (boardModel row column spaceRow spacesToMove playerType)
  (cond ((= spacesToMove 0)
         NIL)
        ((= row spaceRow)
         NIL)
        (t
         (cond ((checkSpacesBetweenRows boardModel row column spaceRow spacesToMove playerType)
                t)
               (t
                NIL)))))

;***************************************************************************
; Function Name: canMoveToSpace
; Purpose: To determine if a die can move from one space to another without any problems
; Parameters: boardModel, the board model that represents the board, dieRow and dieColumn, the row and column of the die to move, spaceRow and
; spaceColumn, the coordinates of the space to move to, and playerType, the player type of the die being moved
; Return value: a boolean that determines whether or not the die is able to successfully travel to the space
; Local variables: dieTopNum, the top number of the die to be moved, rowRolls and columnRolls, the amount of rolls needed to get to the desired
; row and column, and frontalMove and lateralMove, values that determine whether or not a lateral or frontal move is possible
; Algorithm:
; 1. If spaceRow or spaceColumn is greater than 8 or 9, it's invalid positions. Return false
; 2. Initialize dieTopNum as the top number of the die, rowRolls as the number of rolls needed to traverse to the desired row, and columnRolls as
; the number of rolls needed to traverse to the desired column
; 3. If the top number on the die is not equal to the number of rowRolls and columnRolls, a move is not possible. Return false
; 4. Check if the coordinates being moved to on the board are valid. If not, return false
; 5. Check if the die can be moved frontally or laterally at first
; 6. If it can't move either way return false
; 7. If a frontal move is possible, check for a second lateral move. If it isn't possible and the remaining columnRolls isn't 0, a move is
; not possible. Return false
; 8. Otherwise, a move is possible. Return true
; 9. If a lateral move is possible, check for a second frontal move. If it isn't possible and the remaining rowRolls isn't 0, a move is not
; possible. Return false
; 10. Otherwise, a movie is possible. Return true
; 11. If the function has not returned true yet, a move is not possible. Return false
; Assistance received: none
;***************************************************************************

(defun canMoveToSpace (boardModel dieRow dieColumn spaceRow spaceColumn playerType)
  (cond ((or (> spaceRow 8) (> spaceColumn 9))
          NIL))
  (let ((dieTopNum (getDieOnSpaceTopNum boardModel dieRow dieColumn))
        (rowRolls (abs (- spaceRow dieRow)))
        (columnRolls (abs (- spaceColumn dieColumn))))
    (cond ((not (= dieTopNum (+ rowRolls columnRolls)))
           NIL)
          ((or (or (< spaceRow 1) (> spaceRow 8)) (or (< spaceColumn 1) (> spaceColumn 9)))
           NIL)
          (t
           (let ((frontalMove (canMoveFrontally boardModel dieRow dieColumn spaceRow dieTopNum playerType))
                 (lateralMove (canMoveLaterally boardModel dieRow dieColumn spaceColumn dieTopNum playerType)))
             (cond ((and (not frontalMove) (not lateralMove))
                    NIL)
                   (frontalMove
                    (cond ((and (not (canMoveLaterally boardModel spaceRow dieColumn spaceColumn columnRolls playerType)) (not (equal columnRolls 0)))
                           NIL)
                          (t
                           t)))
                   (lateralMove
                    (cond ((and (not (canMoveFrontally boardModel dieRow spaceColumn spaceRow rowRolls playerType)) (not (equal rowRolls 0)))
                           NIL)
                          (t
                           t)))
                   (t
                    NIL)))))))

;***************************************************************************
; Function Name: getKeyDieCoordinates
; Purpose: To get the coordinates of a key die on the board
; Parameters: boardModel, the board model to search for the key die, playerType, the player type of the key die to look for, and rowNumber and columnNumber,
; the numbers of the row and column being searched
; Return value: A list containing the coordinates of the key die
; Local variables: none
; Algorithm:
; 1. Check if rowNumber is 0. If so, the end of the search has been reached and no coordinates were found. Return a list of 0 to indicate such
; 2. Otherwise, check if the die on the coordinates passed into the function is a key die and is of the playerType passed into the function
;    3. If so, this is the key die we're looking for! Return its coordinates in a list
; 4. Otherwise, we will need to recurse into the function:
;    5. If the columnNumber is less than 9, recurse into the function with columnNumber incremented;
;    6. If the columnNumber is 9, recurse into the function with rowNumber decremented and columnNumber as 9
; Assistance received: none
;***************************************************************************

(defun getKeyDieCoordinates (boardModel playerType rowNumber columnNumber)
  (cond ((= rowNumber 0)
         '(0))
        (t
         (cond ((and (isDieOnSpaceKeyDie boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType))
         (list rowNumber columnNumber))
        (t
         (cond ((< columnNumber 9)
                (getKeyDieCoordinates boardModel playerType rowNumber (+ columnNumber 1)))
               ((= columnNumber 9)
                (getKeyDieCoordinates boardModel playerType (- rowNumber 1) 1))))))))

;***************************************************************************
; Function Name: getDieToMoveCoordinates
; Purpose: To get the coordinates of a die that can capture another die on a space
; Parameters: boardModel, the board model to search for the die to move, playerType, the player type of the dice to search for, rowNumber and columnNumber,
; the numbers of the row and column being searched, and spaceRow and spaceColumn, the coordinates of the space to move to
; Return value: A list containing the coordinates of the die to move
; Local variables: none
; Algorithm:
; 1. Check if rowNumber is 0
;    2. If so, there is no die that can move to that space. Return a list of 0 to indicate as such
; 3. Otherwise, check if there is a die on rowNumber and columnNumber and if the die is of the playerType in the parameters
;       3. If so, check if it can move to the space coordinates in the column. If so, return a list containg rowNumber and columnNumber
;    4. If not, we will need to recurse into the function:
;       5. If columnNumber is less than 9, recurse into the function with columnNumber incremented
;       6. If columnNumber is 9, recurse into the function with rowNumber decremented and columnNumber as 1
; Assistance received: none
;***************************************************************************

(defun getDieToMoveCoordinates (boardModel playerType rowNumber columnNumber spaceRow spaceColumn)
  (cond ((= rowNumber 0)
         '(0))
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType))
                (cond ((canMoveToSpace boardModel rowNumber columnNumber spaceRow spaceColumn playerType)
                       (list rowNumber columnNumber))
                      (t
                       (cond ((< columnNumber 9)
                              (getDieToMoveCoordinates boardModel playerType rowNumber (+ columnNumber 1) spaceRow spaceColumn))
                             ((= columnNumber 9)
                              (getDieToMoveCoordinates boardModel playerType (- rowNumber 1) 1 spaceRow spaceColumn))))))
                (t
                 (cond ((< columnNumber 9)
                        (getDieToMoveCoordinates boardModel playerType rowNumber (+ columnNumber 1) spaceRow spaceColumn))
                       ((= columnNumber 9)
                        (getDieToMoveCoordinates boardModel playerType (- rowNumber 1) 1 spaceRow spaceColumn))))))))

;***************************************************************************
; Function Name: captureKeyDieScore
; Purpose: To determine if a key die can be captured or not
; Parameters: boardModel, the board model to search for dice to capture the key die on, and playerType, the player type of the player "calling"
; the function
; Return value: a list containing the coordinates of a die that can capture the key die. If not possible, a list of 0 is returned instead
; Local variables: keyDieCoords, the coordinates of the key die, and potentialDieCatch, coords of a die that can potentially capture it
; Algorithm:
; 1. Determine the playerType in the parameters.
;    2. If it's H (human), initialize keyDieCoords as the coordinate of the Computer's key die
;    3. Get potentialDieCatch and return a list with all the coordinates
;    4. If it's not, it's C (computer). Initialize keyDieCoords as the coordinate of the Human's key die
;    5. Get potentialDieCatch and return a list with all the coordinates
; Assistance received: none
;***************************************************************************

(defun captureKeyDieScore (boardModel playerType)
  (cond ((equal playerType 'H)
         (let* ((keyDieCoords (getKeyDieCoordinates boardModel 'C 8 1))
                (potentialDieCatch (getDieToMoveCoordinates boardModel playerType 8 1 (first keyDieCoords)
                                                            (first (rest keyDieCoords)))))
           (list (first potentialDieCatch) (first (rest potentialDieCatch)) (first keyDieCoords) (first (rest keyDieCoords)))))
        (t
         (let* ((keyDieCoords (getKeyDieCoordinates boardModel 'H 8 1))
                (potentialDieCatch (getDieToMoveCoordinates boardModel playerType 8 1 (first keyDieCoords)
                                                            (first (rest keyDieCoords)))))
           (list (first potentialDieCatch) (first (rest potentialDieCatch)) (first keyDieCoords) (first (rest keyDieCoords)))))))

;***************************************************************************
; Function Name: captureKeySpaceScore
; Purpose: To determine if a key space can be captured or not
; Parameters: boardModel, the board model to search for dice to capture the key die on, and playerType, the player type of the player "calling"
; the function
; Return value: a list containing the coordinates of a die that can capture the key space. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. Determine the playerType in the parameters
;    2. If it's H (human), return the result of getDieToMoveCoordinates on the computer's key space
;    3. If it's not, it's C (computer). Return the result of getDieToMoveCoordinates on the human's key space
; Assistance received: none
;***************************************************************************

(defun captureKeySpaceScore (boardModel playerType)
  (cond ((equal playerType 'H)
         (getDieToMoveCoordinates boardModel playerType 8 1 8 5))
        (t
         (getDieToMoveCoordinates boardModel playerType 8 1 1 5))))

;***************************************************************************
; Function Name: betweenRowsScore
; Purpose: To determine if a die can be moved in between two rows
; Parameters: boardModel, the board model to search for spaces between rows to move to, dieRow and dieColumn, the coordinates of the die to check for moving,
; rowLimit1 and rowLimit2, the rows to search between, and spaceColumn, the column coordinate that remains the same
; Return value: a list containing the coordinates of a die that can move and the space it can move to. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. Check if rowLimit1 is greater than rowLimit2
;    2. If so, check if the die at the coordinates can move to (rowLimit1, columnNumber)
;       3. If so, return a list containing the coordinates of the space to move to
;    4. If not, recurse back into the function but check with the row below rowLimit1 instead
; 5. Check if rowLimit1 is less than rowLimit2
;    6. Repeat steps 2-4, except recurse back into the function with the row above rowLimit1
; 7. If rowLimit1 and rowLimit2 are equal, the die is unable to move anywhere between the rows. Return a list of 0 to indicate such
; Assistance received: none
;***************************************************************************

(defun betweenRowsScore (boardModel playerType dieRow dieColumn rowLimit1 rowLimit2 columnNumber)
  (cond ((> rowLimit1 rowLimit2)
         (cond ((canMoveToSpace boardModel dieRow dieColumn rowLimit1 columnNumber playerType)
                (list rowLimit1 columnNumber))
               (t
                (betweenRowsScore boardModel playerType dieRow dieColumn (- rowLimit1 1) rowLimit2 columnNumber))))
        ((< rowLimit1 rowLimit2)
         (cond ((canMoveToSpace boardModel dieRow dieColumn rowLimit1 columnNumber playerType)
                (list rowLimit1 columnNumber))
               (t
                (betweenRowsScore boardModel playerType dieRow dieColumn (+ rowLimit1 1) rowLimit2 columnNumber))))
        ((= rowLimit1 rowLimit2)
         '(0))))

;***************************************************************************
; Function Name: betweenColumnsScore
; Purpose: To determine if a die can be moved in between two columns
; Parameters: boardModel, the board model to search for spaces between rows to move to, dieRow and dieColumn, the coordinates of the die to check for moving,
; columnLimit1 and columnLimit2, the column to search between, and rowNumber, the row coordinate that remains the same
; Return value: a list containing the coordinates of a die that can move and the space it can move to. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. Check if columnLimit1 is greater than rowLimit2
;    2. If so, check if the die at the coordinates can move to (rowNumber, columnLimit1)
;       3. If so, return a list containing the coordinates of the space to move to
;    4. If not, recurse back into the function but check with the column to the left of columnLimit1 instead
; 5. Check if columnLimit1 is less than columnLimit2
;    6. Repeat steps 2-4, except recurse back into the function with the column to the right of rowLimit1
; 7. If columnLimit1 and columnLimit2 are equal, the die is unable to move anywhere between the rows. Return a list of 0 to indicate such
; Assistance received: none
;***************************************************************************

(defun betweenColumnsScore (boardModel playerType dieRow dieColumn columnLimit1 columnLimit2 rowNumber)
  (cond ((> columnLimit1 columnLimit2)
         (cond ((canMoveToSpace boardModel dieRow dieColumn rowNumber columnLimit1 playerType)
                (list rowNumber columnLimit1))
               (t
                (betweenColumnsScore boardModel playerType dieRow dieColumn (- columnLimit1 1) columnLimit2 rowNumber))))
        ((< columnLimit1 columnLimit2)
         (cond ((canMoveToSpace boardModel dieRow dieColumn rowNumber columnLimit1 playerType)
                (list rowNumber columnLimit1))
               (t
                (betweenColumnsScore boardModel playerType dieRow dieColumn (+ columnLimit1 1) columnLimit2 rowNumber))))
        ((= columnLimit1 columnLimit2)
         '(0))))

;***************************************************************************
; Function Name: blockingRowScore
; Purpose: To determine if a die can be moved in between two rows
; Parameters: boardModel, the board model to search for spaces between rows to move to, rowNumber and columnNumber, the spaces to search for dice to
; move, rowLimit1 and rowLimit2, the columns to search between, and spaceColumn, the column coordinate that remains the same
; Return value: a list containing the coordinates of a die that can move and the space it can move to. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. If rowNumber is 0, there is no die that can move in between rowLimit1 and rowLimit2. Return a list of 0 to indicate such
; 2. Otherwise, continue the checks:
;    3. Check if there is a die on (rowNumber, columnNumber) and if it's of playerType
;       4. If so, initialize rowScoreResult as the result of betweenRowsScore called on the die at (rowNumber, columnNumber)
;       5. Check if the first element of the list is not 0
;          6. If so, the die can move there. Return a list with the dice coordinates and the space coordinates
;       7. Otherwise, recurse back into the function:
;          8. If columnNumber is less than 9, recurse back into the function with columnNumber incremented
;          9. If columnNumber is 9, recurse back into the function with rowNumber decremented and columnNumber at 1
;    10. If not, repeat steps 7-9 to recurse back into the function
; Assistance received: none
;***************************************************************************

(defun blockingRowScore (boardModel playerType rowNumber columnNumber rowLimit1 rowLimit2 spaceColumn)
  (cond ((= rowNumber 0)
         '(0))
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType))
                (let ((rowScoreResult (betweenRowsScore boardModel playerType rowNumber columnNumber rowLimit1 rowLimit2 spaceColumn)))
                (cond ((not (= (first rowScoreResult) 0))
                       (cons rowNumber (cons columnNumber rowScoreResult)))
                      (t
                       (cond ((< columnNumber 9)
                              (blockingRowScore boardModel playerType rowNumber (+ columnNumber 1) rowLimit1 rowLimit2 spaceColumn))
                             ((= columnNumber 9)
                              (blockingRowScore boardModel playerType (- rowNumber 1) 1 rowLimit1 rowLimit2 spaceColumn)))))))
                (t
                 (cond ((< columnNumber 9)
                        (blockingRowScore boardModel playerType rowNumber (+ columnNumber 1) rowLimit1 rowLimit2 spaceColumn))
                       ((= columnNumber 9)
                        (blockingRowScore boardModel playerType (- rowNumber 1) 1 rowLimit1 rowLimit2 spaceColumn))))))))

;***************************************************************************
; Function Name: blockingColumnScore
; Purpose: To determine if a die can be moved in between two columns
; Parameters: boardModel, the board model to search for spaces between columns to move to, rowNumber and columnNumber, the spaces to search for dice to
; move, columnLimit1 and columnLimit2, the columns to search between, and spaceRow, the row coordinate that remains the same
; Return value: a list containing the coordinates of a die that can move and the space it can move to. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. If rowNumber is 0, there is no die that can move in between columnLimit1 and columnLimit2. Return a list of 0 to indicate such
; 2. Otherwise, continue the checks:
;    3. Check if there is a die on (rowNumber, columnNumber) and if it's of playerType
;       4. If so, initialize columnScoreResult as the result of betweenColumnsScore called on the die at (rowNumber, columnNumber)
;       5. Check if the first element of the list is not 0
;          6. If so, the die can move there. Return a list with the dice coordinates and the space coordinates
;       7. Otherwise, recurse back into the function:
;          8. If columnNumber is less than 9, recurse back into the function with columnNumber incremented
;          9. If columnNumber is 9, recurse back into the function with rowNumber decremented and columnNumber at 1
;    10. If not, repeat steps 7-9 to recurse back into the function
; Assistance received: none
;***************************************************************************

(defun blockingColumnScore (boardModel playerType rowNumber columnNumber columnLimit1 columnLimit2 spaceRow)
  (cond ((= rowNumber 0)
         '(0))
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType))
                (let ((columnScoreResult (betweenColumnsScore boardModel playerType rowNumber columnNumber columnLimit1 columnLimit2 spaceRow)))
                (cond ((not (= (first columnScoreResult) 0))
                       (cons rowNumber (cons columnNumber columnScoreResult)))
                      (t
                       (cond ((< columnNumber 9)
                              (blockingColumnScore boardModel playerType rowNumber (+ columnNumber 1) columnLimit1 columnLimit2 spaceRow))
                             ((= columnNumber 9)
                              (blockingColumnScore boardModel playerType (- rowNumber 1) 1 columnLimit1 columnLimit2 spaceRow)))))))
                (t
                 (cond ((< columnNumber 9)
                        (blockingColumnScore boardModel playerType rowNumber (+ columnNumber 1) columnLimit1 columnLimit2 spaceRow))
                       ((= columnNumber 9)
                        (blockingColumnScore boardModel playerType (- rowNumber 1) 1 columnLimit1 columnLimit2 spaceRow))))))))

;***************************************************************************
; Function Name: moveKeyDieScore
; Purpose: To determine if the key die can be moved out of danger
; Parameters: boardModel, the board model to search for spaces on, row and column, the coordinates of the key die, enemyRow and enemyColumn, coordinates of 
; the die that could capture the key die, and playerType, player type of the key die to move
; Return value: a list containing the coordinates of the space to move to. If not possible, a list of 0 is returned instead
; Algorithm:
; 1. Determine if the row and enemyRow are the same
;    2. If so, determine if the key die can move to the row above. If so, return a list of the space's coordinates
;    3. Determine if the key die can move to the row below. If so, return a list of the die's and space's coordinates
;    4. Otherwise, the die cannot be moved. Return a list of 0
; 5. Determine if the column and enemyColumn are the same
;    6. If so, determine if the key die can move to the column to the right. If so, return a list of the space's coordinates
;    7. Determine if the key die can move to the column to the left. If so, return a list of the die's and space's coordinates
;    8. Otherwise, the die cannot be moved. Return a list of 0 
; 9. Otherwise, the die cannot be moved. Return a list of 0
; Assistance received: none
;***************************************************************************

(defun moveKeyDieScore (boardModel row column enemyRow enemyColumn playerType)
  (cond ((= row enemyRow)
         (cond ((canMoveToSpace boardModel row column (+ row 1) column playerType)
                (list (+ row 1) column))
               ((canMoveToSpace boardModel row column (- row 1) column playerType)
                (list (- row 1) column))
               (t
                '(0))))
        ((= column enemyColumn)
         (cond ((canMoveToSpace boardModel row column row (+ column 1) playerType)
                (list row column row (+ column 1)))
               ((canMoveToSpace boardModel row column row (- column 1) playerType)
                (list row column row (- column 1)))
               (t
                '(0))))
        (t
         '(0))))
                
;***************************************************************************
; Function Name: blockKeyDieScore
; Purpose: To determine if a key die needs to be blocked or not
; Parameters: boardModel, the board model to search for dice to block capture, playerType, the player type of the player "calling" the function,
; and opponentType, the opponent's player type
; Return value: a list containing the coordinates of the die to move and the space to move it to. If not possible, a list of 0 is returned instead
; Local variables: keyDieCoords, coordinates of the key die, enemyCaptureScore, the coordinates of a potential die to capture the key die,
; playerCaptureScore, the coordinates of a potential die to capture the die at enemyCaptureScore, columnResults, the result of blockingColumnScore,
; and rowResults, the result of blockingRowScore
; Algorithm:
; 1. Initialize keyDieCoords as the coordinates of the player's key die and enemyCaptureScore as the result of captureKeyDieScore on the enemy's dice
; 2. If enemyCaptureScore returned a list of 0, the key die is not in danger. Return a list of 0
;    3. Otherwise, the key die is in danger and capture needs to be blocked.
;    4. Initialize playerCaptureScore as the result of getDieToMoveCoordinates to the offending die's coordinates
;    5. If the first element of the list is not 0, a move can be made. Return the list
;       6. If it is 0, try to perform a blocking move
;       7. If the offending die and the key die are in the same row, initialize columnResults as the results of blockingColumnScore
;       8. If the first element of the list is not 0, a move can be made. Return the list
;          9. Otherwise, check if the offending die and the key die are in the same column
;             10. If they are, initialize rowResults as the results of blockingRowScore
;             11. If the first element of the list is not 0, a move can be made. Return the list
;                 12. Otherwise, when all else fails, just move the key die away! Or try to...
;                 13. Initialize keyMoveResults as the result of moveKeyDieScore
;                 14. If the first element of the list is not 0, a move can be made. Return the list
;                     15. Otherwise, a move is not possible or necessary. Return a list of 0
; Assistance received: none
;***************************************************************************

(defun blockKeyDieScore (boardModel playerType opponentType)
  (let ((keyDieCoords (getKeyDieCoordinates boardModel playerType 8 1))
        (enemyCaptureScore (captureKeyDieScore boardModel opponentType)))
    (cond ((= (first enemyCaptureScore) 0)
           '(0))
          (t
           (let ((playerCaptureScore (getDieToMoveCoordinates boardModel playerType 8 1 (first enemyCaptureScore) (first (rest enemyCaptureScore)))))
             (cond ((not (= (first playerCaptureScore) 0))
                    (list (first playerCaptureScore) (first (rest playerCaptureScore)) (first enemyCaptureScore) (first (rest enemyCaptureScore))))
                   (t
                    (cond ((= (first keyDieCoords) (first enemyCaptureScore))
                           (let ((columnResults (blockingColumnScore boardModel playerType 8 1 (first (rest keyDieCoords)) (first (rest enemyCaptureScore)) (first keyDieCoords))))
                             (cond ((not (= (first columnResults) 0))
                                    columnResults)
                                   (t
                                    (cond ((= (first (rest keyDieCoords)) (first (rest enemyCaptureScore)))
                                           (let ((rowResults (blockingRowScore boardModel playerType 8 1 (first keyDieCoords) (first enemyCaptureScore) (first (rest keyDieCoords)))))
                                             (cond ((not (= (first rowResults) 0))
                                                    rowResults)
                                                   (t
                                                    (let ((keyMoveResults (moveKeyDieScore boardModel (first keyDieCoords) (first (rest keyDieCoords)) (first enemyCaptureScore) (first (rest enemyCaptureScore)) playerType)))
                                                      (cond ((not (= (first keyMoveResults) 0))
                                                             keyMoveResults)
                                                            (t
                                                             '(0))))))))
                                          (t
                                           '(0)))))))
                    (t
                     '(0))))))))))

;***************************************************************************
; Function Name: blockKeySpaceScore
; Purpose: To determine if a key space capture needs to be blocked or not
; Parameters: boardModel, the board model to search for dice to block capture on, playerType, the type of the player "calling" the function,
; enemyType, the type of the enemy player, and keyRow and keyColumn, the coordinates of the key space
; Return value: a list containing either coordinates to use in a move or 0 if there aren't any possible moves
; Local variables: keyDieCoords, coordinates of the key die, enemyCaptureScore, the coordinates of a potential die to capture the key die,
; playerCaptureScore, the coordinates of a potential die to capture the die at enemyCaptureScore, columnResults, the result of blockingColumnScore,
; and rowResults, the result of blockingRowScore
; Algorithm:
; 1. Initialize enemyCaptureScore as the result of getDieToMoveCoordinates on (keyRow, keyColumn)
; 2. If the first element of enemyCaptureScore is 0, the key space is not in danger. Return a list of 0
;    3. Otherwise, the key space is in danger. Initialize playerCaptureScore as the result of getDieToMoveCoordinates to the offending die's coordinates
;    4. If the first element of the list is not 0, a move can be made. Return the list
;    5. If it is 0, try to perform a blocking move
;       6. If the offending die and the key space are in the same row, initialize columnResults as the results of blockingColumnScore
;       7. If the first element of the list is not 0, a move can be made. Return the list
;          8. Otherwise, check if the offending die and the key space are in the same column
;             9. If they are, initialize rowResults as the results of blockingRowScore
;             10. If the first element of the list is not 0, a move can be made. Return the list
;                 11. Otherwise, a move is not possible or necessary. Return 0
; Assistance received: none
;***************************************************************************

(defun blockKeySpaceScore (boardModel playerType enemyType keyRow keyColumn)
  (let ((enemyCaptureScore (getDieToMoveCoordinates boardModel enemyType 8 1 keyRow keyColumn)))
    (cond ((= (first enemyCaptureScore) 0)
           '(0))
          (t
           (let ((playerCaptureScore (getDieToMoveCoordinates boardModel playerType 8 1 (first enemyCaptureScore) (first (rest enemyCaptureScore)))))
             (cond ((not (= (first playerCaptureScore) 0))
                    (list (first playerCaptureScore) (first (rest playerCaptureScore)) (first enemyCaptureScore) (first (rest enemyCaptureScore))))
                   (t
                    (cond ((= keyRow (first enemyCaptureScore))
                           (let ((columnResults (blockingColumnScore boardModel playerType 8 1 keyColumn (first (rest enemyCaptureScore)) keyRow)))
                             (cond ((not (= (first columnResults) 0))
                                    columnResults)
                                   (t
                                    (cond ((= keyColumn (first (rest enemyCaptureScore)))
                                           (let ((rowResults (blockingRowScore boardModel playerType 8 1 keyRow (first enemyCaptureScore) keyColumn)))
                                             (cond ((not (= (first rowResults) 0))
                                                    rowResults)
                                                   (t
                                                    )))))))))))))))))

;***************************************************************************
; Function Name: captureDieScore
; Purpose: To determine if an enemy die can be captured
; Parameters: boardModel, the board model to search for dice to capture, playerType, the type of the player "calling" the function, enemyType,
; the opponent's player type, and rowNumber and columnNumber, the coordinates of the dice being searched for
; Return value: a list containing either the coordinates of a player's die to move and an enemy's die to capture or a list containing 0
; Local variables: playerCaptureScore, the coordinates of a die that can capture an enemy die
; Algorithm:
; 1. If rowNumber is 0, the board has been searched and no dice can be captured. Return a list with 0
; 2. Otherwise, determine if there is a die at (rowNumber, columnNumber) and if that die is of the opponent's playerType
;    3. If so, initialize playerCaptureScore as the potential coordinates of a die that can capture the opponent's die
;    4. Check if the first element of playerCaptureScore is not 0
;       5. If so, the die can be captured. Return a list with the coordinates
;    6. Otherwise, recurse back into the function:
;       7. If columnNumber is less than 9, recurse into the function with columnNumber incremented
;       8. If columnNumber is 9, recurse into the function with rowNumber decremented and columnNumber set to 1
;       9. If something went wrong somewhere return a list with 0
; 7. If not, recurse back into the function using the same methods as steps 7-9
; Assistance received: none
;***************************************************************************

(defun captureDieScore (boardModel playerType enemyType rowNumber columnNumber)
  (cond ((= rowNumber 0)
         '(0))
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber enemyType))
                (let ((playerCaptureScore (getDieToMoveCoordinates boardModel playerType 8 1 rowNumber columnNumber)))
                  (cond ((not (= (first playerCaptureScore) 0))
                         (list (first playerCaptureScore) (first (rest playerCaptureScore)) rowNumber columnNumber))
                        (t
                         (cond ((< columnNumber 9)
                                (captureDieScore boardModel playerType enemyType rowNumber (+ columnNumber 1)))
                               ((= columnNumber 9)
                                (captureDieScore boardModel playerType enemyType (- rowNumber 1) columnNumber))
                               (t
                                '(0)))))))
               (t
                (cond ((< columnNumber 9)
                       (captureDieScore boardModel playerType enemyType rowNumber (+ columnNumber 1)))
                      ((= columnNumber 9)
                       (captureDieScore boardModel playerType enemyType (- rowNumber 1) columnNumber))
                      (t
                       '(0))))))))

;***************************************************************************
; Function Name: randomMove
; Purpose: To determine a random move to make on the board
; Parameters: boardModel, the board model to make a move on, playerType, the type of the player making the move, rowNumber and columnNumber, the rows and columns
; to search on the board, and randomRow and randomColumn, random coordinates to try to move to
; Return value: a list containing either the coordinates of a die that can move to the random space and the random space itself, or a list of 0
; Local variables: none
; Algorithm:
; 1. If rowNumber is 0, the board has been searched and no dice can move to the space. Recurse into the list with new random coordinates to move to
; 2. Otherwise, determine if there is a die at (rowNumber, columnNumber), if that die is of the player's playerType, and if it can move to (randomRow, randomColumn)
;    3. If so, return a list with those coordinates
;    4. Otherwise, recurse back into the function:
;       5. If columnNumber is less than 9, recurse into the function with columnNumber incremented
;       6. If columnNumber is 9, recurse into the function with rowNumber decremented and columnNumber set to 1
;       7. If something went wrong somewhere return a list with 0
; Assistance received: none
;***************************************************************************

(defun randomMove (boardModel playerType rowNumber columnNumber randomRow randomColumn)
  (cond ((= rowNumber 0)
         (randomMove boardModel playerType 8 1 (+ (random 8) 1) (+ (random 9) 1)))
        (t
         (cond ((and (isDieOn boardModel rowNumber columnNumber) (isDieOnSpacePlayerType boardModel rowNumber columnNumber playerType) (canMoveToSpace boardModel rowNumber columnNumber randomRow randomColumn playerType))
                (list rowNumber columnNumber randomRow randomColumn))
               (t
                (cond ((< columnNumber 9)
                       (randomMove boardModel playerType rowNumber (+ columnNumber 1) randomRow randomColumn))
                      ((= columnNumber 9)
                       (randomMove boardModel playerType (- rowNumber 1) 1 randomRow randomColumn))
                      (t
                       '(0))))))))
                    
;***************************************************************************
; Function Name: printMove
; Purpose: To print the move that was just made to the window
; Parameters: dieNameBefore and dieNameAfter, the name of the die before and after it was moved, dieRow and dieColumn, the coordinates of the die before
; it was moved, spaceRow and spaceColumn, the coordinates of the die after it was moved, and direction, the direction that the die first moved in
; Return value: T
; Local variables: rowRolls and columnRolls, the amount of rolls needed to move frontally and laterally to the space desired
; Algorithm:
; 1. Initialize rowRolls and columnRolls
; 2. Print the name of the die before it was moved, and print that it was rolled from the square that it was initially on
; 3. Use the direction passed into the function to determine how the die was first moved
; 4. Check if there was a 90 degree turn made by checking if rowRolls or columnRolls is not 0. If so, print the number of spaces moved after the turn
; 5. Print the name of the die after it was moved and where it is now located
; Assistance received: none
;***************************************************************************

(defun printMove (dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn direction)
  (let ((rowRolls (abs (- spaceRow dieRow)))
        (columnRolls (abs (- spaceColumn dieColumn))))
    (princ dieNameBefore)
    (princ " was rolled from square (")
    (princ dieRow)
    (princ ",")
    (princ dieColumn)
    (princ ") ")
    (cond ((equal direction "frontally")
           (princ "frontally by ")
           (princ rowRolls)
           (cond ((not (= columnRolls 0))
                  (princ " and laterally by ")
                  (princ columnRolls))
                 (t
                  )))
          ((equal direction "laterally")
           (princ "laterally by ")
           (princ columnRolls)
           (cond ((not (= rowRolls 0))
                  (princ " and frontally by ")
                  (princ rowRolls))
                  (t
                   ))))
    (princ " to square (")
    (princ spaceRow)
    (princ ",")
    (princ spaceColumn)
    (princ "). The die is now ")
    (princ dieNameAfter)
    (printMessage ".")))

;***************************************************************************
; Function Name: printStrategy
; Purpose: To print the strategy behind a move to the window
; Parameters: strategy, the strategy to print
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print the name of the die the computer picked and its original coordinates
; 2. Print the reason for it moving by looking into the strategy and printing based on the word in the parameters
; 3. Print the amount of spaces it was rolled by and the reason to move it that way by looking into the strategy
; 4. Print the new name of the die and its new coordinates
; Assistance received: none
;***************************************************************************

(defun printStrategy (dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn direction strategy)
  (let ((rowRolls (abs (- spaceRow dieRow)))
        (columnRolls (abs (- spaceColumn dieColumn))))
  (princ "The Computer picked ")
  (princ dieNameBefore)
  (princ " at (")
  (princ dieRow)
  (princ ",")
  (princ dieColumn)
  (princ ") to move because ")
  (cond ((equal strategy "keyDieCapture")
         (printMessage "it was within distance of the human's key die."))
        ((equal strategy "keySpaceCapture")
         (printMessage "it was within distance of the human's key space."))
        ((equal strategy "blockKeyDie")
         (printMessage "the key die was in danger of being captured, and needed to block the capture."))
        ((equal strategy "blockKeySpace")
         (printMessage "the key space was in danger of being captured, and needed to block the capture."))
        ((equal strategy "dieCapture")
         (printMessage "it was within distance of a human's die that was able to be captured."))
        (t
         (printMessage "the computer could not determine a decisive move to make, so it is making a move at random.")))
  (princ "It rolled it ")
  (cond ((equal direction "frontally")
         (princ "frontally by ")
         (princ rowRolls)
         (cond ((> columnRolls 0)
                (princ " and laterally by ")
                (princ columnRolls))
               (t
                )))
        (t
         (princ "laterally by ")
         (princ columnRolls)
         (cond ((> rowRolls 0)
                (princ " and frontally by ")
                (princ rowRolls))
               (t
                ))))
    (princ " because ")
    (cond ((equal strategy "keyDieCapture")
           (printMessage "those movements allowed it to move to the coordinates of the key die."))
          ((equal strategy "keySpaceCapture")
           (printMessage "those movements allowed it to move to the coordinates of the key space."))
          ((equal strategy "blockKeyDie")
           (printMessage "those movements were able to block a key die capture."))
          ((equal strategy "blockKeySpace")
           (printMessage "those movements were able to block a key space capture."))
          ((equal strategy "dieCapture")
           (printMessage "those movements allowed it to move to the coordinates of the die it wants to capture."))
          (t
           (printMessage "the die was able to move that way without any problems.")))
    (princ "The die is now ")
    (princ dieNameAfter)
    (princ " at (")
    (princ spaceRow)
    (princ ",")
    (princ spaceColumn)
    (printMessage ").")))
          
; -----------------Human Stuff-------------------

;***************************************************************************
; Function Name: humanPlay
; Purpose: To let the human make a move on the board
; Parameters: boardModel, the board model to make a move on
; Return value: a board model (list of lists) that implements the move the human makes on it
; Local variables: dieRow and dieColumn, the coordinates of the die to move, spaceRow and spaceColumn, the coordinates of the space to move to, spacesToMove, the
; amount of spaces the die is allowed to move by, rowRolls and columnRolls, the amount of rolls needed to traverse to the desired space, lateralMove and frontalMove,
; boolean values that determine if the die can move laterally or frontally at first, and secondFrontalMove and secondLateralMove, boolean values that determine if the
; die can move after a potential 90 degree turn
; Algorithm:
; 1. Ask the user for the coordinates of the die they want to move and the space they want to move to. Store the results in dieRow, dieColumn,
; spaceRow, and spaceColumn
; 2. Check if the coordinates entered actually have a die on it
;    3. If not, print such and recurse back into the function
; 4. Check if the die on the coordinates entered are of the H player type
;    5. If not, print such and recurse back into the function
; 6. Check if there is a die on the space coordinates entered and if that die is of the C player type
;    7. If so, print such and recurse back into the function
; 8. Otherwise, continue checking:
;    9. Initialize spacesToMove as the top number on the die and rowRolls and columnRolls as the amount of spaces needed to traverse across the board
;    10. Check if spacesToMove is equal to rowRolls + columnRolls
;        11. If not, the number on the top of the die is not enough to traverse to the space. Print so and recurse back into the function
;        12. Otherwise, continue checking:
;            13. Initalize frontalMove and lateralMove as booleans that determine if the die can move frontally or laterally from its space
;            14. Check if the die cannot move frontally or laterally
;                15. If so, print such and recurse back into the function
;            16. Otherwise, continue checking:
;                17. Initialize secondLateralMove and secondFrontalMove as booleans that determine if the die can move frontally or laterally
;                after a potential 90 degree turn (if it cannot move that way and it needs to roll any amount of spaces, the check fails)
;                18. Check if the die cannot move frontally and then laterally or laterally and then frontally
;                    19. If so, print such and recurse back into the function
;                20. Otherwise, we're good to go! Call humanMakesMove
; Assistance received: none
;***************************************************************************

(defun humanPlay (boardModel)
  (askForHelp boardModel)
  (let ((dieRow (askForInput "Enter the row coordinate of the die you want to move (1-8): " 1 8))
        (dieColumn (askForInput "Enter the column coordinate of the die you want to move (1-9): " 1 9))
        (spaceRow (askForInput "Enter the row coordinate of the space you want to move to (1-8): " 1 8))
        (spaceColumn (askForInput "Enter the column coordinate of the space you want to move to (1-9): " 1 9)))
    (cond ((not (isDieOn boardModel dieRow dieColumn))
           (printMessage "There is no die on the coordinates entered for the die to move. Please enter coordinates for a die.")
           (humanPlay boardModel))
          ((not (isDieOnSpacePlayerType boardModel dieRow dieColumn 'H))
           (printMessage "The die on the space is not a die you can move. Please enter coordinates for a die you can move.")
           (humanPlay boardModel))
          ((and (isDieOn boardModel spaceRow spaceColumn) (not (isDieOnSpacePlayerType boardModel spaceRow spaceColumn 'C)))
           (printMessage "There is already a die on that space. Please enter coordinates for a space you can move to.")
           (humanPlay boardModel))
          (t
           (let ((spacesToMove (getDieOnSpaceTopNum boardModel dieRow dieColumn))
                 (rowRolls (abs (- spaceRow dieRow)))
                 (columnRolls (abs (- spaceColumn dieColumn))))
             (cond ((not (= spacesToMove (+ rowRolls columnRolls)))
                    (printMessage "The number on the top of the die is not enough to move to the space, please enter a different position.")
                    (humanPlay boardModel))
                   (t
                    (let ((lateralMove (canMoveLaterally boardModel dieRow dieColumn spaceColumn spacesToMove 'H))
                          (frontalMove (canMoveFrontally boardModel dieRow dieColumn spaceRow spacesToMove 'H)))
                      (cond ((not (or lateralMove frontalMove))
                             (printMessage "The die cannot be rolled to that space from its current position. Please enter a different die or space.")
                             (humanPlay boardModel))
                            (t
                             ;; For second moves, a second move is not possible if you cannot move in that direction after a 90 degree turn and the column/rowRolls
                             ;; isn't 0. The negation of all that, however, will be true.
                             (let ((secondFrontalMove (not (and (not (canMoveFrontally boardModel dieRow spaceColumn spaceRow rowRolls 'H)) (not (= rowRolls 0)))))
                                   (secondLateralMove (not (and (not (canMoveLaterally boardModel spaceRow dieColumn spaceColumn columnRolls 'H)) (not (= columnRolls 0))))))
                               (cond ((not (or (and frontalMove secondLateralMove) (and lateralMove secondFrontalMove)))
                                      (printMessage "The die cannot be rolled to that space from its current position. Please enter a different die or space.")
                                      (humanPlay boardModel))
                                     (t
                                      (humanMakesMove boardModel dieRow dieColumn spaceRow spaceColumn lateralMove frontalMove secondFrontalMove secondLateralMove)
                                      )))))))))))))
                                        
;***************************************************************************
; Function Name: humanMakesMove
; Purpose: To make the move that the human wants to make on the board
; Parameters: boardModel, the board model to make a move on, dieRow and dieColumn, the coordinates of the die, spaceRow and spaceColumn, the coordinates of the
; space to move to, and lateralMove, frontalMove, secondLateralMove, and secondFrontalMove, boolean values that determine if moves in certain directions is possible
; Return value: a list of lists that represents the board model with the move made on the board
; Local variables: none
; Algorithm:
; 1. Determine if it is possible to make both a frontal move and then a second lateral move and a lateral move and then a second frontal move
;    2. If so, ask which direction to move in
;    3. Make a move in that direction
; 4. Determine if it is possible to make a frontal move and then a second lateral move
;    5. If so, make a move in that direction
; 6. Otherwise, it's a lateral move and then a frontal move, make a move in that direction
; 7. Return the updated board model
; Assistance received: none
;***************************************************************************

(defun humanMakesMove (boardModel dieRow dieColumn spaceRow spaceColumn lateralMove frontalMove secondFrontalMove secondLateralMove)
  (let ((dieNameBefore (getNameOfDieOnSpace boardModel dieRow dieColumn)))
  (cond ((and (and lateralMove secondFrontalMove) (and frontalMove secondLateralMove))
         (cond ((= (askForInput "Which direction do you want to move in? Enter 1 for frontally or 2 for laterally: " 1 2) 1)
                (let ((dieNameAfter (getNameOfDieOnSpace (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally") spaceRow spaceColumn)))
                  (printMove dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "frontally")
                  (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally")))
                (t
                 (let ((dieNameAfter (getNameOfDieOnSpace (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally") spaceRow spaceColumn)))
                  (printMove dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "laterally")
                   (makeMove (makeMove boardModel dieRow dieColumn spaceColumn "laterally") dieRow spaceColumn spaceRow "frontally")))))
         ((and frontalMove secondlateralMove)
          (let ((dieNameAfter (getNameOfDieOnSpace (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally") spaceRow spaceColumn)))
            (printMove dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "frontally")
            (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally")))
         ((and lateralMove secondFrontalMove)
          (let ((dieNameAfter (getNameOfDieOnSpace (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally") spaceRow spaceColumn)))
            (printMove dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "laterally")
            (makeMove (makeMove boardModel dieRow dieColumn spaceColumn "laterally") dieRow spaceColumn spaceRow "frontally")))
        ;; If something inexplicably went wrong somewhere, just return the regular board model.
        (t
         boardModel))))

;***************************************************************************
; Function Name: getHelp
; Purpose: To get help from the computer player in recommending a move
; Parameters: boardModel, the board model being played on
; Return value: T
; Local variables: keyDieResult, the result of captureKeyDieScore, keySpaceResult, the result of captureKeySpaceScore, blockKeyDieResult, the
; result of blockKeyDieScore, blockKeySpaceResult, the result of blockKeySpaceScore, captureDieResult, the result of captureDieScore, and
; randomResult, the result of randomMove
; Algorithm:
; 1. Compute captureKeyDieScore, store results in keyDieResult
; 2. Check if the first element of the results is not 0
;    3. If so, call recommendMove
; 4. Otherwise, compute captureKeySpaceScore, store the results in keySpaceResult
; 5. Check if the first element of the results is not 0
;    6. If so, call recommendMove
; 7. Otherwise, compute blockKeyDieScore, store the results in blockKeyDieResult
; 8. Check if the first element of the results is not 0
;    9. If so, call recommendMove
; 10. Otherwise, compute blockKeySpaceScore, store the results in blockKeySpaceResult
; 11. Check if the first element of the results is not 0
;     12. If so, call recommendMove
; 13. Otherwise, compute captureDieScore, store the results in captureDieResult
; 14. Check if the first element of the results is not 0
;     15. If so, call recommendMove
; 16. Otherwise, compute randomMove, store the results in randomResult
; 17. Call computerMakesMove with the coordinates in the list and return
; Assistance received: none
;***************************************************************************

(defun getHelp (boardModel)
  (let ((keyDieResult (captureKeyDieScore boardModel 'H)))
    (cond ((not (= (first keyDieResult) 0))
           (recommendMove boardModel (first keyDieResult) (first (rest keyDieResult)) (first (rest (rest keyDieResult)))
                          (first (rest (rest (rest keyDieResult)))) "keyDieCapture"))
          (t
           (let ((keySpaceResult (captureKeySpaceScore boardModel 'H)))
             (cond ((not (= (first keySpaceResult) 0))
                    (recommendMove boardModel (first keySpaceResult) (first (rest keySpaceResult)) 8 5 "keySpaceCapture"))
                   (t
                    (let ((blockKeyDieResult (blockKeyDieScore boardModel 'H 'C)))
                      (cond ((not (= (first blockKeyDieResult) 0))
                             (recommendMove boardModel (first blockKeyDieResult) (first (rest blockKeyDieResult)) (first (rest (rest blockKeyDieResult)))
                                            (first (rest (rest (rest blockKeyDieResult)))) "blockKeyDie"))
                            (t
                             (let ((blockKeySpaceResult (blockKeySpaceScore boardModel 'H 'C 1 5)))
                               (cond ((not (= (first blockKeySpaceResult) 0))
                                      (recommendMove boardModel (first blockKeySpaceResult) (first (rest blockKeySpaceResult)) (first (rest (rest blockKeySpaceResult)))
                                                     (first (rest (rest (rest blockKeySpaceResult)))) "blockKeySpace"))
                                     (t
                                      (let ((captureDieResult (captureDieScore boardModel 'H 'C 8 1)))
                                        (cond ((not (= (first captureDieResult) 0))
                                               (recommendMove boardModel (first captureDieResult) (first (rest captureDieResult))
                                                              (first (rest (rest captureDieResult))) (first (rest (rest (rest captureDieResult))))
                                                              "dieCapture"))
                                              (t
                                               (let ((randomResult (randomMove boardModel 'H 8 1 (+ (random 8) 1) (+ (random 9) 1))))
                                                 (recommendMove boardModel (first randomResult) (first (rest randomResult))
                                                                (first (rest (rest randomResult))) (first (rest (rest (rest randomResult)))) "random"
                                                                ))))))))))))))))))

;***************************************************************************
; Function Name: recommendMove
; Purpose: To print a move recommendation to the window
; Parameters: boardModel, the board model being played on, dieRow and dieColumn, coordinates of the die to move, spaceRow and spaceColumn,
; coordinates of the space to move to, and strategy, the strategy behind the recommendation
; Return type: T
; Local variables: spacesToMove, the top number of the die on (dieRow,dieColumn), rowRolls and columnRolls, the rolls needed to traverse the board,
; dieNameBefore, the name of the die that is recommended to move, frontalMove and lateralMove, boolean values that determine if a die can be moved
; at first, and secondLateralMove and secondFrontalMove, boolean values that determine if a die can be moved after a potential 90 degree turn
; Algorithm:
; 1. Initialize spacesToMove, calculate rowRolls and columnRolls, and determine all the boolean values with the appropriate functions
; 2. Print the name of the die to move and its coordinates
; 3. Print the reasoning of that die to move by looking at strategy and describing it
; 4. Print which way it recommends rolling by looking at the boolean values of frontalMove/lateralMove/secondLateralMove/secondFrontalMove
; 5. Print the reasoning behind moving that way by looking at strategy once again and describing it
; Assistance received: none
;***************************************************************************

(defun recommendMove (boardModel dieRow dieColumn spaceRow spaceColumn strategy)
  (let ((spacesToMove (getDieOnSpaceTopNum boardModel dieRow dieColumn))
        (rowRolls (abs (- spaceRow dieRow)))
        (columnRolls (abs (- spaceColumn dieColumn))))
    (let ((dieNameBefore (getNameOfDieOnSpace boardModel dieRow dieColumn))
          (frontalMove (canMoveFrontally boardModel dieRow dieColumn spaceRow spacesToMove 'H))
          (lateralMove (canMoveLaterally boardModel dieRow dieColumn spaceColumn spacesToMove 'H))
          ;; For second moves, a second move is not possible if you cannot move in that direction after a 90 degree turn and the column/rowRolls isn't 0.
          ;; The negation of all that, however, will be true.
          (secondLateralMove (not (and (not (canMoveLaterally boardModel spaceRow dieColumn spaceColumn columnRolls 'H)) (not (= columnRolls 0)))))
          (secondFrontalMove (not (and (not (canMoveFrontally boardModel dieRow spaceColumn spaceRow rowRolls 'H)) (not (= rowRolls 0))))))
      (princ "The computer recommends moving ")
      (princ dieNameBefore)
      (princ " at (")
      (princ dieRow)
      (princ ",")
      (princ dieColumn)
      (princ ") because ")
      (cond ((equal strategy "keyDieCapture")
             (printMessage "it is within distance of the computer's key die."))
            ((equal strategy "keySpaceCapture")
             (printMessage "it is within distance of the computer's key space."))
            ((equal strategy "blockKeyDie")
             (printMessage "the key die is in danger of being captured, and needs to be blocked."))
            ((equal strategy "blockKeySpace")
             (printMessage "the key space is in danger of being captured, and needs to be blocked."))
            ((equal strategy "dieCapture")
             (printMessage "it is within distance of a computer's die that is able to be captured."))
            (t
             (printMessage "the computer could not determine a decisive move to make, so it is making a move at random.")))
      (princ "It recommends rolling ")
      (cond ((and (and frontalMove secondLateralMove) (and lateralMove secondFrontalMove))
             ;; Randomly decide to move frontally or laterally
             (cond ((= (random 2) 0)
                    ; Frontally
                    (princ "frontally by ")
                    (princ rowRolls)
                    (cond ((not (= columnRolls 0))
                           (princ " and laterally by ")
                           (princ columnRolls))
                          (t
                           )))
                   (t
                    ; Laterally
                    (princ "laterally by ")
                    (princ columnRolls)
                    (cond ((not (= rowRolls 0))
                           (princ " and frontally by ")
                           (princ rowRolls))
                          (t
                           )))))
            ((and frontalMove secondLateralMove)
             (princ "frontally by ")
             (princ rowRolls)
             (cond ((not (= columnRolls 0))
                    (princ " and laterally by ")
                    (princ columnRolls))
                   (t
                    )))
            ((and lateralMove secondFrontalMove)
             (princ "laterally by ")
             (princ columnRolls)
             (cond ((not (= rowRolls 0))
                    (princ " and frontally by ")
                    (princ rowRolls))
                   (t
                    ))))
      (princ " because ")
      (cond ((equal strategy "keyDieCapture")
             (printMessage "it can capture the key die with this move."))
            ((equal strategy "keySpaceCapture")
             (printMessage "it can capture the key space with this move."))
            ((equal strategy "blockKeyDie")
             (printMessage "the key die capture will be blocked with this move."))
            ((equal strategy "blockKeySpace")
             (printMessage "the key space capture will be blocked with this move."))
            ((equal strategy "dieCapture")
             (printMessage "the die will be captured with this move."))
            (t
             (printMessage "the die is able to move this way without any problems."))))))

;***************************************************************************
; Function Name: askForHelp
; Purpose: To ask the human if they want help on a recommendation
; Parameters: boardModel, the board model to potentially get help on
; Return value: T
; Local variables: inputResult, the number the human inputs as an answer to the question
; Algorithm:
; 1. Ask the user if they want help or not
; 2. If they do, call getHelp and then recurse into the function
; 3. If they don't, return
; Assistance received: none
;***************************************************************************

(defun askForHelp (boardModel)
  (let ((inputResult (askForInput "Would you like a move recommendation? Enter 1 to get one or 2 to make a move: " 1 2)))
    (cond ((= inputResult 1)
           (getHelp boardModel)
           (askForHelp boardModel))
          (t
           ))))

; -----------------Computer Stuff-------------------

;***************************************************************************
; Function Name: computerPlay
; Purpose: To let the computer make a move on the board
; Parameters: boardModel, the board model to make a move on
; Return value: a board model (list of lists) that implements the move the computer makes on it
; Local variables: keyDieResult, the result of captureKeyDieScore, keySpaceResult, the result of captureKeySpaceScore, blockKeyDieResult, the
; result of blockKeyDieScore, blockKeySpaceResult, the result of blockKeySpaceScore, captureDieResult, the result of captureDieScore, and
; randomResult, the result of randomMove
; Algorithm:
; 1. Compute captureKeyDieScore, store results in keyDieResult
; 2. Check if the first element of the results is not 0
;    3. If so, call computerMakesMove with the coordinates in the list and return
; 4. Otherwise, compute captureKeySpaceScore, store the results in keySpaceResult
; 5. Check if the first element of the results is not 0
;    6. If so, call computerMakesMove with the coordinates in the list and return
; 7. Otherwise, compute blockKeyDieScore, store the results in blockKeyDieResult
; 8. Check if the first element of the results is not 0
;    9. If so, call computerMakesMove with the coordinates in the list and return
; 10. Otherwise, compute blockKeySpaceScore, store the results in blockKeySpaceResult
; 11. Check if the first element of the results is not 0
;     12. If so, call computerMakesMove with the coordinates in the list and return
; 13. Otherwise, compute captureDieScore, store the results in captureDieResult
; 14. Check if the first element of the results is not 0
;     15. If so, call computerMakesMove with the coordinates in the list and return
; 16. Otherwise, compute randomMove, store the results in randomResult
; 17. Call computerMakesMove with the coordinates in the list and return
; Assistance received: none
;***************************************************************************

(defun computerPlay (boardModel)
  (let ((keyDieResult (captureKeyDieScore boardModel 'C)))
    (cond ((not (= (first keyDieResult) 0))
           (computerMakesMove boardModel (first keyDieResult) (first (rest keyDieResult)) (first (rest (rest keyDieResult)))
                              (first (rest (rest (rest keyDieResult)))) "keyDieCapture"))
          (t
           (let ((keySpaceResult (captureKeySpaceScore boardModel 'C)))
             (cond ((not (= (first keySpaceResult) 0))
                    (computerMakesMove boardModel (first keySpaceResult) (first (rest keySpaceResult)) 1 5 "keySpaceCapture"))
                   (t
                    (let ((blockKeyDieResult (blockKeyDieScore boardModel 'C 'H)))
                      (cond ((not (= (first blockKeyDieResult) 0))
                             (computerMakesMove boardModel (first blockKeyDieResult) (first (rest blockKeyDieResult)) (first (rest (rest blockKeyDieResult)))
                                                (first (rest (rest (rest blockKeyDieResult)))) "blockKeyDie"))
                            (t
                             (let ((blockKeySpaceResult (blockKeySpaceScore boardModel 'C 'H 8 5)))
                               (cond ((not (= (first blockKeySpaceResult) 0))
                                      (computerMakesMove boardModel (first blockKeySpaceResult) (first (rest blockKeySpaceResult)) (first (rest (rest blockKeySpaceResult)))
                                                         (first (rest (rest (rest blockKeySpaceResult)))) "blockKeySpace"))
                                     (t
                                      (let ((captureDieResult (captureDieScore boardModel 'C 'H 8 1)))
                                        (cond ((not (= (first captureDieResult) 0))
                                              (computerMakesMove boardModel (first captureDieResult) (first (rest captureDieResult)) (first (rest (rest captureDieResult)))
                                                                 (first (rest (rest (rest captureDieResult)))) "dieCapture"))
                                              (t
                                               (let ((randomResult (randomMove boardModel 'C 8 1 (+ (random 8) 1) (+ (random 9) 1))))
                                                 (computerMakesMove boardModel (first randomResult) (first (rest randomResult)) (first (rest (rest randomResult)))
                                                                    (first (rest (rest (rest randomResult)))) "random"))))))))))))))))))
          
;***************************************************************************
; Function Name: computerMakesMove
; Purpose: To make the move the computer wants to make on the board
; Parameters: boardModel, the board model to make a move on, dieRow and dieColumn, the coordinates of the die to move, spaceRow and spaceColumn, coordinates
; of the space to move to, and strategy, the strategy that the computer used to make the move
; Return value: a board model (list of lists) that implements the move the computer makes on it
; Local variables: spacesToMove, the top number of the die to move, rowRolls and columnRolls, the amount of rolls needed to traverse the board,
; dieNameBefore, the name of the die before it's moved, frontalMove and lateralMove, booleans that determine which way the die can move at first,
; secondFrontalMove and secondLateralMove, booleans that determine if the die can move in a 90 degree turn, newBoardModel, the board model once the
; move is made on it, and dieNameAfter, the name of the die after it is moved
; Algorithm:
; 1. Initialize spacesToMove as the top number on the die that is being moved
; 2. Calculate the number of rowRolls and columnRolls needed to traverse the board
; 3. Store the name of the die before it is moved to dieNameBefore
; 4. Determine if the die can move frontally or laterally from the die coordinates, then again after a 90 degree turn
; 5. If a frontal and lateral move is possible, randomly choose which direction to go in
; 6. Make the move
; 7. Store the updated board model with the move implemented in newBoardModel
; 8. Store the new name of the die in dieNameAfter
; 9. Print the strategy for the move using printStrategy
; 10. Return newBoardModel
; Assistance received: none
;***************************************************************************

(defun computerMakesMove (boardModel dieRow dieColumn spaceRow spaceColumn strategy)
  (let ((spacesToMove (getDieOnSpaceTopNum boardModel dieRow dieColumn))
        (rowRolls (abs (- spaceRow dieRow)))
        (columnRolls (abs (- spaceColumn dieColumn))))
    (let ((dieNameBefore (getNameOfDieOnSpace boardModel dieRow dieColumn))
          (frontalMove (canMoveFrontally boardModel dieRow dieColumn spaceRow spacesToMove 'C))
          (lateralMove (canMoveLaterally boardModel dieRow dieColumn spaceColumn spacesToMove 'C))
          ;; For second moves, a second move is not possible if you cannot move in that direction after a 90 degree turn and the column/rowRolls isn't 0.
          ;; The negation of all that, however, will be true.
          (secondLateralMove (not (and (not (canMoveLaterally boardModel spaceRow dieColumn spaceColumn columnRolls 'C)) (not (= columnRolls 0)))))
          (secondFrontalMove (not (and (not (canMoveFrontally boardModel dieRow spaceColumn spaceRow rowRolls 'C)) (not (= rowRolls 0))))))
    (cond ((and (and frontalMove secondLateralMove) (and lateralMove secondFrontalMove))
           ;; Randomly decide a frontal or lateral direction to move in first
           (cond ((= (random 2) 0)
                  ;; Move frontally first
                  (let ((newBoardModel (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally")))
                    (let ((dieNameAfter (getNameOfDieOnSpace newBoardModel spaceRow spaceColumn)))
                      (printStrategy dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "frontally" strategy)
                      newBoardModel)))
                 (t
                  ;; Move laterally first
                  (let ((newBoardModel (makeMove (makeMove boardModel dieRow dieColumn spaceColumn "laterally") dieRow spaceColumn spaceRow "frontally")))
                    (let ((dieNameAfter (getNameOfDieOnSpace newBoardModel spaceRow spaceColumn)))
                      (printStrategy dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "laterally" strategy)
                      newBoardModel)))))
          (t
           (cond ((and frontalMove secondLateralMove)
                  (let ((newBoardModel (makeMove (makeMove boardModel dieRow dieColumn spaceRow "frontally") spaceRow dieColumn spaceColumn "laterally")))
                        (let ((dieNameAfter (getNameOfDieOnSpace newBoardModel spaceRow spaceColumn)))
                          (printStrategy dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "frontally" strategy)
                          newBoardModel)))
                 ((and lateralMove secondFrontalMove)
                  (let ((newBoardModel (makeMove (makeMove boardModel dieRow dieColumn spaceColumn "laterally") dieRow spaceColumn spaceRow "frontally")))
                        (let ((dieNameAfter (getNameOfDieOnSpace newBoardModel spaceRow spaceColumn)))
                          (printStrategy dieNameBefore dieNameAfter dieRow dieColumn spaceRow spaceColumn "laterally" strategy)
                          newBoardModel)))
                 (t
                  boardModel)))))))
                  
         

; -----------------Other Stuff-------------------

;***************************************************************************
; Function Name: printMessage
; Purpose: To print a message to the window
; Parameters: message, the message that will be printed
; Return value: T
; Local variables: none
; Algorithm: 
; 1. Call princ on the message passed into the function
; Assistance received: none
;***************************************************************************

(defun printMessage (message)
  (princ message)
  (terpri))

;***************************************************************************
; Function Name: askForInput
; Purpose: To ask the user for (valid) input
; Parameters: message, the message to print before asking for input, minNum, the minimum number for valid input, and maxNum, the maximum number
; for valid input
; Return value: T
; Local variables: none
; Algorithm:
; 1. Print the message that goes with the input
; 2. Call validateInput on the same parameters of the function but also include read to actually get the input
; Assistance received: none
;***************************************************************************

(defun askForInput (message minNum maxNum)
  (princ message)
  (validateInput message minNum maxNum (read)))
  
;***************************************************************************
; Function Name: validateInput
; Purpose: To validate the input passed into the parameters
; Parameters: message, the message to print before asking for input (if it needs to ask again), minNum, the minimum number for valid input,
; maxNum, the maximum number for valid input, and input, the actual input to check
; Return value: the number that the user input into the function
; Local variables: none
; Algorithm:
; 1. If input is less than minNum, ask for input again
; 2. If input is greater than maxNum, ask for input again
; 3. Otherwise, it's valid input. Return the number
; Assistance received: none
;***************************************************************************

(defun validateInput (message minNum maxNum input)
  (cond ((< input minNum)
         (askForInput message minNum maxNum))
        ((> input maxNum)
         (askForInput message minNum maxNum))
        (t
         input)))