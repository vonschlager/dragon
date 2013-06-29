<apply template="admin">
<a class="btn btn-primary" href="/admin/wpis/dodaj">Nowy wpis</a>
<table class="table table-hover">
  <thead>
    <tr>
      <th>Tytuł</th>
      <th>Treść</th>
      <th style="width: 10%">Rodzaj</th>
      <th style="width: 10%" colspan="2"><center>Akcje</center></th>
    </tr>
  </thead>
  <tbody>
<posts>
  <tr>
    <td><a href="/wpis/pokaz/${id}"><title/></a></td>
    <td><body/></td>
    <td><kind/></td>
    <td><a class="btn btn-small" href="/admin/wpis/edytuj/${id}">Edytuj</a></td>
    <td><a class="btn btn-small btn-danger" href="/admin/wpis/usun/${id}">Usuń</a></td>
  </tr>
</posts>
  </tbody>
</table>
</apply>
