package horses

class Position(var x: Int,var y:Int) {
	override def toString = {
		"(" + this.x + "," + this.y +")"
	}
}

abstract class HorseColor
case class White() extends HorseColor
case class Black() extends HorseColor

/**
 * 
 * ver video: http://www.youtube.com/watch?v=Yqhfneh3NLc
 *
 */
class Horse (var pos: Position, var color:HorseColor ) {
	override def toString = {
		pos+print(color)
	}
	
	def print(c: HorseColor): String = c match {
		case White() => "B"
		case Black() => "N"

	}
	
	def position: Position = pos
	
	def move(newPos: Position): Unit = {
		this.pos = newPos
	}
	
	def equals (other: Horse):Boolean = {
		(this.position.x equals other.position.x) &&
		(this.position.y equals other.position.y) &&
		this.color      == other.color
	}
}


class horsesGame(tablero: Array[Position], inicio: Array[Horse], val paraGanar: Array[Horse]) {

	def isValidInitialState: Boolean = {
		
		this.inicio.foreach  {
			h: Horse =>
			println(h);
		}
		true
	}
	
	def pr(t:(Boolean, Boolean)): Unit = {
		println("Movimiento válido: "+ t._1 + " Gana el juego: " + t._2)
	}
	/**
	 * 
	 * @param horse
	 * @param newPosition
	 * @return
	 */
	def move(horse:Horse, newPosition:Position): (Boolean,Boolean) = {
		val dx:Int = (horse.position.x - newPosition.x).abs
		val dy:Int = (horse.position.y - newPosition.y).abs
		
//		println("dx =" + dx + " dy="+dy)
		
		// Verifica que el movimiento que se quiere hacer es correcto
		if (dx==2&&dy==1||dx==1&&dy==2) {
//			println("primer punto de control pasado")
			
			// verifica que el Horse pasado por par�metro est� realmente en el tablero
			var isInGame = false
			inicio.foreach((h:Horse) => isInGame = (isInGame || h==horse))
//			inicio.foreach((h:Horse) => println(h))
//			println("tamaño: " +inicio.length)
//			println("esta en juego:" + isInGame)
			
			if (isInGame) {
				// verifica que la posici�n a la que se quiere ir no est� ocupada
				var isCellOcupied = false
				inicio.foreach((h:Horse) => isCellOcupied = (h.position equals newPosition) || isCellOcupied)
				
				if (!isCellOcupied) {
					// Verifica que la posici�n a la que se quiere ir est� en el tablero
					var cellExists = false
					tablero.foreach((h:Position) => cellExists = (h equals newPosition) || cellExists )
					
					horse.move(newPosition)
					
					// verifica que haya ganado el juego
					var matches = 0;
					inicio.foreach((hi:Horse) => 
						paraGanar.foreach((hf:Horse) => matches = matches + {if(hi equals hf) 1 else 0})
					)

					if (matches==4) (true,true)
					else (true, false)
					
				} 
				else
					(false, false)
			}
			else
				(false,false)
		}
		else
			(false,false)
	}
	
}

object horsesApp extends Application {
	override def main(args : Array[String]): Unit = {
		
		val pos1 = new Position(2,3);
//		println("pos1:" + pos1.toString);
		
		val c1 = new Horse(pos1,Black())
		val c2 = new Horse(new Position(4,5),White())
		
//		println(c1)
//		println(c2)
		
		val tablero = Array {
			new Position (1,1); 
			new Position (2,1);
			new Position (3,1);
			new Position (4,1);
			new Position (1,2);
			new Position (2,2);
			new Position (3,2);
			new Position (2,3);
			new Position (3,3);
			new Position (3,4)
		}
		
		
		val BH1 = new Horse(new Position(1,1),Black())
		val BH2 = new Horse(new Position(3,2),Black())
		val WH1 = new Horse(new Position(2,2),White())
		val WH2 = new Horse(new Position(3,4),White())
		
		val WH3 = new Horse(new Position(3,4),White())
		
		val inicio = Array (
			BH1,
			BH2,
			WH1,
			WH2
		)
		
		val fin = Array (
			new Horse(new Position(1,1),White()),
			new Horse(new Position(3,2),White()),
			new Horse(new Position(2,2),Black()),
			new Horse(new Position(3,4),Black())
		)
	
		print(WH2.equals(WH3))
		
		val game = new horsesGame(tablero, inicio, fin)
		

		val moves = List[(Horse,Position)](
			(BH1, new Position(2,3)),
			(BH1, new Position(3,1)),
			(BH1, new Position(1,2)),
			(BH1, new Position(3,3)),
			(BH1, new Position(2,1)),
			(WH1, new Position(4,1)),
			(WH1, new Position(3,3)),
			(WH1, new Position(1,2)),
			(WH1, new Position(3,1)),
			(WH2, new Position(2,2)),
			(WH2, new Position(4,1)),
			(WH2, new Position(3,3)),
			(WH2, new Position(1,2)),
			(BH1, new Position(3,3)),
			(BH1, new Position(4,1)),
			(BH1, new Position(2,2)),
			(BH1, new Position(3,4)),
			(WH2, new Position(3,3)),
			(WH2, new Position(4,1)),
			(WH2, new Position(2,2)),
			(WH1, new Position(1,2)),
			(WH1, new Position(3,3)),
			(WH1, new Position(4,1)),
			(BH2, new Position(1,1)),
			(BH2, new Position(2,3)),
			(BH2, new Position(3,1)),
			(BH2, new Position(1,2)),
			(BH2, new Position(3,3)),
			(BH2, new Position(2,1)),
			(WH1, new Position(3,3)),			
			(WH1, new Position(1,2)),	
			(WH1, new Position(3,1)),
			(WH1, new Position(2,3)),
			(WH1, new Position(1,1)),
			(WH1, new Position(3,2)),
			(WH2, new Position(4,1)),			
			(WH2, new Position(3,3)),			
			(WH2, new Position(1,2)),	
			(WH2, new Position(3,1)),
			(WH2, new Position(2,3)),
			(WH2, new Position(1,1)),
			(BH2, new Position(3,3)),
			(BH2, new Position(4,1)),
			(BH2, new Position(2,2))			
		)
		
		moves.foreach( (t:(Horse,Position)) => game.pr(game.move(t._1,t._2)))		
	}
}
