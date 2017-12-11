import json
from zijin.papers.models import Paper

r = redis.Redis(host='localhost', port=6379, db=4, decode_responses=True)

all_visits = [i for i in r.keys() if i.startswith("visiturl")]

with open("/tmp/user_click.log", "w") as f:
    for i in all_visits:
        url_histories = r.lrange(i, 0, -1)
        # print(url_histories) # ok
        for j in url_histories:
            ins = r.hgetall(j)
            if ins != {}:
                ins.update({"username": i.replace("visiturl:", "")})
                f.write(str(ins))
                print("{} done".format(i))

with open("/tmp/paper_info.backup", "w") as f:
    queryset = Paper.objects.all()
    lst = [{"id": i.id, "title": i.title, "vote_count": i.votes, "oppose_count":i.opposes, "hotness": i.hotness, "follower_count": i.followers_count, "comment_count": i.comment_count, "note_count": i.note_count, "create_time": i.created_at.timestamp(), "pub_time": i.papertime, "hascode?": hasattr(i, "reproductions"), "tag_count": i.tags.count(), "tags_follower_count": sum([j.followers_count for j in i.tags.all()]), "userid": i.user.id, "username": i.user.username} for i in queryset]
    for i in lst:
        f.write(str(i))
        print("{} done".format(i["id"]))

with open("/tmp/user_info.backup", "w") as f:
    queryset = User.objects.all()
    lst = [{"id": i.id, "name": i.username, "academic_background": i.profile.academic_background, "research_direction":i.profile.research_direction, "follower_count": i.profile.followers_count, "note_count": i.profile.note_count, "paper_count": i.profile.paper_count, "tag_count": i.follow_tags.count(), "user_vec": get_user_vec(i.username).tostring()} for i in queryset if hasattr(i, "profile") if hasattr(get_user_vec(i.username), "tostring")]
    for i in lst:
        f.write(str(i))
        print("{} done".format(i["id"]))
