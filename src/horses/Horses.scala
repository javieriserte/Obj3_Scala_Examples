package horses

/**
 * ver video: http://www.youtube.com/watch?v=Yqhfneh3NLc
 */

abstract class HorseColor 
case class White() extends HorseColor {
	override def toString():String = {"W"}
}

case class Black() extends HorseColor{
	override def toString():String = {"B"}
}

class Horse (var pos: Position, var color:HorseColor ) {
	override def toString = {
		pos.toString + color.toString
	}

	def position: Position = pos
	
	def move(newPos: Position): Unit = {
		this.pos = newPos
	}
	
	def equals (other: Horse):Boolean = {
//		(this.position.x equals other.position.x) &&
//		(this.position.y equals other.position.y) &&
		(this.position equals other.position) &&
		this.color == other.color
	}
}


class horsesGame(board: Array[Position], currentState: Array[Horse], val toWin: Array[Horse]) {

	def printMove(t:(Boolean, Boolean)): Unit = {
		println("Valid Move: "+ t._1 + " | Wins: " + t._2)
	}
	/**
	 * Verifica que la posici�n a la que se quiere ir est� en el tablero
	 * @param p
	 * @return
	 */
	def cellExist(p:Position):Boolean = {
//		var cellExists = false
//		this.tablero.foreach((boardPos:Position) => cellExists = (boardPos equals p) || cellExists )
//		cellExists
		this.board.foldLeft(false){(acum,value) => 
//		println(value, p, value equals p)
		(value equals p) || acum }
	}
	
    /**
     *  verifica que la posici�n a la que se quiere ir no est� ocupada	
     * @param p
     * @return
     */
	def isCellOcupied(p:Position):Boolean = {
//		var isCellOcupied=false
//		inicio.foreach((h:Horse) => isCellOcupied = (h.position equals p) || isCellOcupied)
//		isCellOcupied
		currentState.foldLeft(false)((acum,value) => (value.position equals p) || acum)
	}
	
	/**
	 * Verifica que haya ganado el juego
	 * @return
	 */
	def winTheGame():Boolean = {
		currentState.foldLeft(true)((acum_a,hi:Horse) => 
			acum_a && 
		    (toWin.foldLeft(false)((acum,hf:Horse) => 
		    	(hi equals hf) || acum
		    	)
		    )
		)
//		var matches = 0;
//		inicio.foreach((hi:Horse) => 
//			paraGanar.foreach((hf:Horse) => matches = matches + {if(hi equals hf) 1 else 0})
//		)
//		(matches==4)
	}
	
	/**
	 * verifica que el Horse pasado por par�metro est� realmente en el tablero
	 * @param horse
	 * @return
	 */
	def isInGame(horse:Horse):Boolean = {
//		var isInGame = false
//		inicio.foreach((h:Horse) => isInGame = (isInGame || h==horse))
		currentState.foldLeft(false)((acum, value) => acum || value==horse)
		
	}
	
	/**
	 * Verifica que el movimiento que se quiere hacer es correcto
	 */
	def isValidMove(horse:Horse, newPosition:Position): Boolean = {
		val dx:Int = (horse.position.x - newPosition.x).abs
		val dy:Int = (horse.position.y - newPosition.y).abs
		(dx==2&&dy==1||dx==1&&dy==2) 
	}
	/**
	 * 
	 * @param horse
	 * @param newPosition
	 * @return
	 */
	def move(horse:Horse, newPosition:Position): (Boolean,Boolean) = {
//		if (isValidMove(horse,newPosition)) {
//			if (isInGame(horse)) {
//				if (!isCellOcupied(newPosition)) {
//					if (cellExist(newPosition)) { 
//						horse.move(newPosition)
//						(true,winTheGame())					
//					} else 
//						(false, false)
//				} 
//				else
//					(false, false)
//			}
//			else
//				(false,false)
//		}
//		else
//			(false,false)
		
		if ((isValidMove(horse,newPosition)) && 
			(isInGame(horse))  && 
			(!isCellOcupied(newPosition)) && 
			(cellExist(newPosition))) 
		    { 
				horse.move(newPosition)
				(true,winTheGame())					
		    } else 
			    (false, false)
	}
	
}

class Position(var x: Int,var y:Int) {
	override def toString = {
		"(" + this.x + "," + this.y +")"
	}
	def equals(other:Position):Boolean = {
		(this.x equals other.x) &&
		(this.y equals other.y)
	}
}

object horsesApp extends Application {
	override def main(args : Array[String]): Unit = {
		
		val pos1 = new Position(2,3);
		val c1 = new Horse(pos1,Black())
		val c2 = new Horse(new Position(4,5),White())

		val board = Array ( new Position (1,1), new Position (2,1), new Position (3,1), new Position (4,1),
			                  new Position (1,2), new Position (2,2), new Position (3,2),
			                                      new Position (2,3), new Position (3,3),
			                                                          new Position (3,4)
		)
		
		val BH1 = new Horse(new Position(1,1),Black())
		val BH2 = new Horse(new Position(3,2),Black())
		val WH1 = new Horse(new Position(2,2),White())
		val WH2 = new Horse(new Position(3,4),White())
		
		val initialState = Array (BH1, BH2, WH1, WH2 )
		
		val fin = Array ( new Horse(new Position(1,1),White()),
			              new Horse(new Position(3,2),White()),
			              new Horse(new Position(2,2),Black()),
			              new Horse(new Position(3,4),Black()) )
	
		val game = new horsesGame(board, initialState, fin)

		val moves = List[(Horse,Position)](
			(BH1, new Position(2,3)), (BH1, new Position(3,1)), (BH1, new Position(1,2)), (BH1, new Position(3,3)), (BH1, new Position(2,1)),
			(WH1, new Position(4,1)), (WH1, new Position(3,3)), (WH1, new Position(1,2)), (WH1, new Position(3,1)), (WH2, new Position(2,2)), (WH2, new Position(4,1)), (WH2, new Position(3,3)), (WH2, new Position(1,2)), 
			(BH1, new Position(3,3)), (BH1, new Position(4,1)), (BH1, new Position(2,2)), (BH1, new Position(3,4)), 
			(WH2, new Position(3,3)), (WH2, new Position(4,1)), (WH2, new Position(2,2)), 
			(WH1, new Position(1,2)), (WH1, new Position(3,3)), (WH1, new Position(4,1)),
			(BH2, new Position(1,1)), (BH2, new Position(2,3)), (BH2, new Position(3,1)), (BH2, new Position(1,2)), (BH2, new Position(3,3)), (BH2, new Position(2,1)),
			(WH1, new Position(3,3)), (WH1, new Position(1,2)), (WH1, new Position(3,1)), (WH1, new Position(2,3)), (WH1, new Position(1,1)), (WH1, new Position(3,2)),
			(WH2, new Position(4,1)), (WH2, new Position(3,3)), (WH2, new Position(1,2)), (WH2, new Position(3,1)), (WH2, new Position(2,3)), (WH2, new Position(1,1)),
			(BH2, new Position(3,3)), (BH2, new Position(4,1)), (BH2, new Position(2,2))			
		)
		
		moves.foreach( (t:(Horse,Position)) => game.printMove(game.move(t._1,t._2)))
	}
}
