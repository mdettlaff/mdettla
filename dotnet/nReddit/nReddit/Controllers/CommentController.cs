using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;
using System.Data;

namespace nReddit.Controllers
{
    public class CommentController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Comment/Create

        [Authorize]
        public ActionResult Create()
        {
            return View();
        }

        //
        // POST: /Comment/Create

        [HttpPost]
        [Authorize]
        public ActionResult Create(Comment comment)
        {
            if (ModelState.IsValid)
            {
                int submissionID = (int)Session["submissionID"];
                comment.Username = User.Identity.Name;
                Submission submission =
                    db.Submissions.Include("Comments").Single(s => s.SubmissionID == submissionID);
                db.Comments.Add(comment);
                submission.Comments.Add(comment);
                db.Entry(submission).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", "Submission", new { id = submissionID });
            }
            return View(comment);
        }
    }       
}
