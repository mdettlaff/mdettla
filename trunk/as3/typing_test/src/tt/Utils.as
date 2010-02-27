package tt {

    public class Utils {

        private static const PL:String = "ąĄćĆęĘłŁńŃóÓśŚżŻźŹ";
        private static const EN:String = "aAcCeElLnNoOsSzZzZ";

        public static function shavePlChars(withPlChars:String):String {
            var withoutPlChars:String = "";
            for (var i:int = 0; i < withPlChars.length; i++) {
                var c:String = withPlChars.charAt(i);
                var plIndex:int = PL.indexOf(c);
                withoutPlChars += plIndex != -1 ? EN.charAt(plIndex) : c;
            }
            return withoutPlChars;
        }

        public static function containsPlChars(s:String,
                index:int = 0, array:Array = null):Boolean {
            for (var i:int = 0; i < s.length; i++) {
                if (PL.indexOf(s.charAt(i)) != -1) {
                    return true;
                }
            }
            return false;
        }
    }
}
