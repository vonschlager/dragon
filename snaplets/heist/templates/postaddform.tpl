<apply template="index">
<form method="post" action="/post/add">
  <fieldset>
    <legend>Posty</legend>
    <label>Title</label>
    <input type="text" name="title" size="20" placeholder="title..."/>
    <label>Body</label>
    <textarea name="body" size="20" placeholder="body..."></textarea>
    <label>Kind</label>
    <select name="kind">
      <kinds>
        <option value="${kind}"><name/></option>
      </kinds>
    </select><br/>
    <button class="btn" type="submit">Send</button>
  </fieldset>
</form>
</apply>
