package tetris {

    import flash.events.Event;

    public class TetrisEvent extends Event {

        public static const TETROMINO_LANDED:String = "tetrominoLanded";
        public static const LINES_DESTROYED:String = "linesDestroyed";
        public static const NEW_GAME:String = "newGame";

        public var tetromino:Tetromino;
        public var destroyedLinesCount:int;

        public function TetrisEvent(eventName:String,
                tetromino:Tetromino = null, destroyedLinesCount:int = 0) {
            super(eventName, true);
            this.tetromino = tetromino;
            this.destroyedLinesCount = destroyedLinesCount;
        }
    }
}
