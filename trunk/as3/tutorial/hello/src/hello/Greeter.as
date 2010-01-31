package hello {

    public class Greeter {
        /**
         * Defines the names that should receive a proper greeting.
         */
        public static var validNames:Array = ["Sammy", "Frank", "Dean"];

        /**
         * Builds a greeting string using the given name.
         */
        public function getGreetingText(userName:String = ""):String {
            var greeting:String;
            if (userName == "") {
                greeting = "Witaj. Podaj swoją nazwę użytkownika, po czym "
                    + "naciśnij Enter";
            } else if (validName(userName)) {
                greeting = "Witaj, " + userName + ".";
            } else {
                greeting = "Przykro mi " + userName
                    + ", nie ma cię na liście.";
            }
            return greeting;
        }

        /**
         * Checks whether a name is in the validNames list.
         */
        public static function validName(inputName:String = ""):Boolean {
            trace("Znani użytkownicy: " + validNames);
            return validNames.indexOf(inputName) > -1;
        }
    }
}
