<apply template="index">
<form method="post" action="/post/add">
  <table>
    <tr>
      <td>title:</td><td><input type="text" name="title" size="20" /></td>
    </tr>
    <tr>
      <td>body:</td><td><input type="text" name="body" size="20" /></td>
    </tr>
    <tr>
      <td>kind:</td><td>
        <select name="kind">
          <kinds/>
        </select>
      </td>
    </tr>
    <tr>
      <td></td><td><input type="submit" value="Send" /></td>
    </tr>
  </table>
</form>
</apply>
