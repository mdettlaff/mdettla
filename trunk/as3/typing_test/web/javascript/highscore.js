function updateHighscoreTable() {
    var url = "ajax/highscore_table.php";
    var request;

    function onResponse() {
        var i;
        var hsTable;
        var entries;
        var entryElement;
        var speed;
        var correctness;

        if (request.readyState != 4 || request.status != 200) {
            return;
        }

        hsTable = "<h3>Najlepsze wyniki</h3>";
        hsTable += "<table class=\"highscore-table\">";
        entries =
            request.responseXML.documentElement.getElementsByTagName("entry");
        hsTable += "<tr>";
        hsTable += "<th>Miejsce</th>";
        hsTable += "<th>Nazwa użytkownika</th>";
        hsTable += "<th>Prędkość<br />(znaki/min)</th>";
        hsTable += "<th>Prędkość<br />(słowa/min)</th>";
        hsTable += "<th>Poprawność</th>";
        hsTable += "</tr>";
        for (i = 0; i < entries.length; i++) {
            if (i % 2 === 0) {
                hsTable += "<tr style=\"background: #F1F1DD\">";
            } else {
                hsTable += "<tr>";
            }
            hsTable += "<td>" + (i + 1) + "</td>";
            entryElement = entries[i].getElementsByTagName("username");
            hsTable += "<td style=\"text-align: left\">";
            hsTable += entryElement[0].firstChild.nodeValue;
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("speed");
            speed = entryElement[0].firstChild.nodeValue;
            hsTable += "<td><b>" + speed.replace('.', ',') + "</b></td>";
            hsTable += "<td>";
            hsTable += ((speed / 5).toFixed(1) + "").replace('.', ',');
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("correctness");
            correctness = entryElement[0].firstChild.nodeValue;
            hsTable += "<td>";
            hsTable += correctness.replace('.', ',') + "%";
            hsTable += "</td>";
            hsTable += "</tr>";
        }
        hsTable += "</table>";
        document.getElementById("highscoreTableArea").innerHTML = hsTable;
    }

    if (XMLHttpRequest) {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        request = new XMLHttpRequest();
    } else {
        // code for IE5, IE6
        request = new ActiveXObject("Microsoft.XMLHTTP");
    }
    request.onreadystatechange = onResponse;
    url += "?r=" + new Date().getTime(); // prevent IE from caching results
    request.open("GET", url, true);
    request.send(null);
}
