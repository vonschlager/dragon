<apply template="admin">
<div>
  <a href="/admin/post/add">Dodaj post</a>
</div>
<div class="span7 offset2">
<table class="table table-hover table-bordered">
  <thead>
  </thead>
  <tbody>
<posts>
  <tr>
    <td><a href="/post/view/${postid}"><title/></a></td>
    <td><a href="/admin/post/edit/${postid}">Edytuj</a></td>
    <td><a href="/admin/post/delete/${postid}">Usuń</a></td>
  </tr>
</posts>
  </tbody>
</table>
</div>
</apply>
