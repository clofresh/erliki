-record(wiki, {page, body, last_author}).
-record(user, {name, password_hash}).
-record(role, {name, user}).
-record(session, {id, user_name}).

% whitelist
-record(acl, {url_pattern, method, who}).
