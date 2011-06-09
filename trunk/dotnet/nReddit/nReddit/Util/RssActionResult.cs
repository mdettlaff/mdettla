using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.IO;

namespace nReddit.Util
{
    public class RssActionResult : ActionResult
    {
        private string content = "Empty";

        public RssActionResult(string content)
        {
            this.content = content;
        }

        public override void ExecuteResult(ControllerContext context)
        {
            context.HttpContext.Response.ContentType = "text/xml";

            TextWriter writer = context.HttpContext.Response.Output;
            writer.Write(content);
            writer.Close();
        }
    }
}