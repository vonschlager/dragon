<apply template="admin">
<table class="table table-hover">
  <thead>
    <tr>
      <th>Nick</th>
      <th>Email</th>
      <th>WWW</th>
      <th>Treść</th>
      <th>Data</th>
      <th style="width: 10%" colspan="2"><center>Akcje</center></th>
    </tr>
  </thead>
  <tbody>
<guestbook>
  <tr>
    <td><nick/></a></td>
    <td><email/></td>
    <td><www/></td>
    <td><body/></td>
    <td><creation/></td>
    <td><a class="btn btn-small" href="/admin/ksiega/edytuj/${id}">Edytuj</a></td>
    <td><a class="btn btn-small btn-danger" href="/admin/ksiega/usun/${id}">Usuń</a></td>
  </tr>
</guestbook>
  </tbody>
</table>
</apply>
