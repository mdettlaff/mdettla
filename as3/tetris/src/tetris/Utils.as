package tetris {

    public class Utils {

        public static function createArray2D(sizeX:int, sizeY:int):Array {
            var array:Array = new Array(sizeY);
            for (var i:int = 0; i < sizeY; i++) {
                array[i] = new Array(sizeX);
            }
            return array;
        }
    }
}
