<apply template="index">
<form method="post" action="/navbar/add">
  <table>
    <tr>
      <td>name:</td><td><input type="text" name="name" size="20" /></td>
    </tr>
    <tr>
      <td>link:</td><td>
        <select name="link">
          <kinds/>
        </select>
      </td>
    </tr>
    <tr>
      <td>order:</td><td><input type="text" name="order" size="20" /></td>
    </tr>
    <tr>
      <td></td><td><input type="submit" value="Send" /></td>
    </tr>
  </table>
</form>
</apply>
