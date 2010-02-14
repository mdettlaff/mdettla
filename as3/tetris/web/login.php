<?php include 'template/top.php' ?>

<form name="login_form" method="POST" action="login_validate.php">
  <table border="0" cellspacing="4">
    <tr>
      <td>Nazwa użytkownika:</td>
      <td>
        <input name="username" type="text" />
      </td>
    </tr>
    <tr>
      <td>Hasło:</td>
      <td>
        <input name="passwd" type="password" />
      </td>
    </tr>
    <tr>
      <td />
      <td>
        <input type="submit" value="Zaloguj" />
      </td>
    </tr>
  </table>
</form>

<?php include 'template/bottom.php' ?>
