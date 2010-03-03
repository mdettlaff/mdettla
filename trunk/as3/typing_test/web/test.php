<html>
<head>
  <meta http-equiv="Content-type" content="text/html; charset=utf-8">
  <meta name="Language" content="pl">
  <title>Test prędkości online</title>

<script>

function showHighscoreTable() {
    var url = "ajax/highscore_table.php";
    var xmlhttp;

    function onResponse() {
        var i;
        var hsTable;
        var entryElement;
        var speed;

        if (xmlhttp.readyState != 4 || xmlhttp.status != 200) {
            return;
        }

        hsTable = "<table border=\"1\">";
        entries =
            xmlhttp.responseXML.documentElement.getElementsByTagName("entry");
        hsTable += "<tr>";
        hsTable += "<th>Nazwa użytkownika</th>";
        hsTable += "<th>Prędkość (znaki/min)</th>";
        hsTable += "<th>Prędkość (słowa/min)</th>";
        hsTable += "<th>Poprawność</th>";
        hsTable += "</tr>";
        for (i = 0; i < entries.length; i++) {
            hsTable += "<tr>";
            entryElement = entries[i].getElementsByTagName("username");
            hsTable += "<td>" + entryElement[0].firstChild.nodeValue + "</td>";
            entryElement = entries[i].getElementsByTagName("speed");
            speed = entryElement[0].firstChild.nodeValue;
            hsTable += "<td>" + speed.replace('.', ',') + "</td>";
            hsTable += "<td>";
            hsTable += ((speed / 5) + "").replace('.', ',');
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("correctness");
            hsTable += "<td>";
            hsTable += entryElement[0].firstChild.nodeValue + "%"
            hsTable += "</td>";
            hsTable += "</tr>";
        }
        hsTable += "</table>";
        document.getElementById("highscoreTableArea").innerHTML = hsTable;
    }

    if (window.XMLHttpRequest) {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp = new XMLHttpRequest();
    } else {
        // code for IE5, IE6
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }
    xmlhttp.onreadystatechange = onResponse;
    xmlhttp.open("GET", url, true);
    xmlhttp.send(null);
}

showHighscoreTable();

</script>

</head>
<body bgcolor="#F5F5F5">
    <h3>Najlepsze wyniki</h3>
    <div id="highscoreTableArea"></div>
<button onClick="showHighscoreTable()">Odśwież</button>
</body>
</html>
