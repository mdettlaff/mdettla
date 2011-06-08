using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;

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
    }
}
