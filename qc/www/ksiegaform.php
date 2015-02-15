<form action="ksiegacheck.php" method=post>
<input type=hidden name=data value="<? echo(date("d.m.Y")."</b> o godzinie <b>".date("H:i")); ?>">
<input type=hidden name=action value=check>
<table border=0>
<tr><td align=right>
autor:</td><td><input type=text name=autor></td></tr><tr><td align=right>
e-mail:</td><td><input type=text name=email></td></tr><tr><td valign=top>
komentarz:</td><td><textarea name=komentarz cols=32 rows=5></textarea>
</td></tr>
</table>
<br><br>
<input type=submit value=Wy¶lij>
<input type=reset value="Wyczy¶æ">
</form>
