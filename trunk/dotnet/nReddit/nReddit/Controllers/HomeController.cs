using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;
using nReddit.Util;

namespace nReddit.Controllers
{
    public class HomeController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Home/

        public ActionResult Index()
        {
            ICollection<Submission> submissions = db.Submissions.ToList();
            return View(submissions);
        }

        public ActionResult Rss()
        {
            ICollection<Submission> submissions = db.Submissions.ToList();
            string feed = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
            feed += "<rss version=\"2.0\">";
            feed += "<channel>";
            feed += "<title>nReddit</title>";
            foreach (Submission submission in submissions) {
                feed += "<item>";
                feed += "<title>";
                feed += submission.Title;
                feed += "</title>";
                feed += "<link>" + submission.Url + "</link>";
                feed += "<description>" + submission.Text + "</description>";
                feed += "</item>";
            }
            feed += "</channel>";
            feed += "</rss>";
            return new RssActionResult(feed);
        }
    }
}
