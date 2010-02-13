package tetris {

    public class Utils {

        public function Utils() {
            throw new Error("Do not instantiate this class!");
        }

        public static function createArray2D(sizeX:int, sizeY:int):Array {
            var array2D:Array = new Array(sizeY);
            for (var i:int = 0; i < sizeY; i++) {
                array2D[i] = new Array(sizeX);
            }
            return array2D;
        }

        public static function randInt(n:int):int {
            return n * Math.random();
        }

        public static function shuffle(array:Array):Array {
            var original:Array = array.slice(0, array.length);
            var shuffled:Array = [];
            while (original.length > 0) {
                shuffled.push(original.splice(randInt(original.length), 1));
            }
            return shuffled;
        }
    }
}
