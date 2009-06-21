-record(wiki, {id, page, body, author, timestamp}).
-record(user, {name, password_hash}).
-record(role, {name, user}).
-record(session, {id, user_name}).

% whitelist
-record(acl, {url_pattern, method, who}).
