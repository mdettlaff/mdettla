package tt {

    import flash.events.KeyboardEvent;
    import flash.system.Capabilities;
    import flash.utils.ByteArray;

    public class PolishChars {

        private static const ISO_8859_2_PRINTABLE:String =
            " Ą˘Ł¤ĽŚ§¨ŠŞŤŹ­ŽŻ°ą˛ł´ľśˇ¸šşťź˝žżŔÁÂĂÄĹĆÇČÉĘËĚÍÎĎ" +
            "ĐŃŇÓÔŐÖ×ŘŮÚŰÜÝŢßŕáâăäĺćçčéęëěíîďđńňóôőö÷řůúűüýţ˙";

        private var char:ByteArray = new ByteArray();
        private var isPrevUTFMarker:Boolean = false;

        public function charFrom(event:KeyboardEvent):String {
            if (Capabilities.os.indexOf("Linux") != -1) {
                return charForLinux(event);
            } else {
                return charForWindows(event);
            }
        }

        private function charForLinux(event:KeyboardEvent):String {
            char.writeByte(event.charCode);
            var isUTFMarker:Boolean =
                event.charCode > 194 && event.charCode < 198;
            if (isUTFMarker) {
                isPrevUTFMarker = true;
                return null;
            } else {
                var c:String;
                char.position = 0;
                if (isPrevUTFMarker) {
                    c = char.readMultiByte(2, "UTF-8");
                } else {
                    if (event.charCode > 160 && event.charCode < 256) {
                        c = ISO_8859_2_PRINTABLE.charAt(event.charCode - 160);
                    } else {
                        c = String.fromCharCode(event.charCode);
                    }
                }
                char.position = 0;
                isPrevUTFMarker = false;
                return c;
            }
        }

        private function charForWindows(event:KeyboardEvent):String {
            const EN:String = "aAcCeElLnNoOsSzZxX";
            const PL:String = "ąĄćĆęĘłŁńŃóÓśŚżŻźŹ";
            var index:int;
            if (event.altKey) {
                index = EN.indexOf(String.fromCharCode(event.charCode));
                if (index != -1) {
                    return String.fromCharCode(PL.charCodeAt(index));
                }
            }
            return String.fromCharCode(event.charCode);
        }
    }
}
