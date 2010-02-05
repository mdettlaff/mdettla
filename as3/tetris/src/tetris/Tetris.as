package tetris {

    import tetris.Tetromino;
    import tetris.TetrominoCreator;

    import flash.utils.Timer;
    import flash.events.Event;
    import flash.events.TimerEvent;
    import flash.events.KeyboardEvent;
    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.ui.Keyboard;

    public class Tetris extends Sprite {

        private var tetromino:Tetromino;

        public function Tetris(mainContainer:DisplayObjectContainer) {
            var tetrominoFactory:TetrominoCreator = new TetrominoCreator();
            tetromino = tetrominoFactory.addTetromino(
                    TetrominoCreator.L, mainContainer);

            var timer:Timer = new Timer(1000, 0);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();

            mainContainer.addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);
        }

        private function timerHandler(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function keyHandler(event:KeyboardEvent):void {
            switch (event.keyCode) {
                case Keyboard.LEFT:
                    tetromino.moveLeft();
                    break;
                case Keyboard.RIGHT:
                    tetromino.moveRight();
                    break;
                case Keyboard.DOWN:
                    tetromino.moveDown();
                    break;
                case Keyboard.UP:
                    tetromino.rotateClockwise();
                    break;
            }
        }
    }
}
