<template>

  <div class="con" v-if="imgSpcail" >
    <div :class="['item',imgSite]">
      <img :src="imgSpcail" alt="" >
    </div>
  </div>
</template>


<script>
  import brg from '../../../utils/middle'
  import {playVoice} from '../../../utils/audio'
  import {mapGetters} from 'vuex'



    const chiUrl = require('../../../assets/nav_icon/chi/chi.gif')
    const huUrl = require('../../../assets/nav_icon/chi/hu.gif')
    const pengUrl = require('../../../assets/nav_icon/chi/peng.gif')
    const gangUrl = require('../../../assets/nav_icon/chi/gang.gif')
    const paoUrl = require('../../../assets/nav_icon/chi/pao.gif')
    const zimoUrl = require('../../../assets/nav_icon/chi/zimo.gif')

  export default {
    data() {
      return {
        show: false,
        content: '112',
        timer: null,
        imgSpcail:'',
        imgSite:'site1'
      }
    },
    mounted() {
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj
        if (id === 510) {
          this.doAction(msg)
        }
      })
    },
    created() {

    },
    computed: {
      ...mapGetters({
        'user_info':'user_info',
        'members':'members'
      })
    },
    methods: {
      doAction(msg) {

        if (msg.type === 2) {

          let con = msg.content.split(',')
          let aa = con[0].substr(1, con[0].length - 2)
          let type = String.fromCharCode(aa)
          if (type == 'o') {
            setTimeout(() => {
              playVoice(con[1])
            }, 200)
            playVoice('chupai')
          } else if (type == 'e') {
            playVoice('chi')
            this.showAction(chiUrl,con[2])
          } else if (type == 'p') {
            playVoice('peng')
            this.showAction(pengUrl,con[2])
          } else if (type == 'k') {
            playVoice('gang')
            this.showAction(gangUrl,con[2])
          }else if (type == 'w') {
            playVoice('hu')
            this.showAction(huUrl,con[2])
          }else if (type == 's') {
            playVoice('pao')
            this.showAction(paoUrl,con[3])
          }


        }
      },
      showAction(type,uid) {
        let myId = this.user_info.id;
        if(this.members.length<4){
          return false
        }


        let user = this.members.find(v => v.PlayerId == myId)
        let other = this.members.find(v => v.PlayerId == uid)


        let classObj = ''


        if (user.position - other.position == 1 || user.position - other.position == -3) {
          classObj = 'site4'
          console.log('上家')
        } else if (other.position - user.position == 1 || other.position - user.position == -3) {
          console.log('下家')
          classObj = 'site2'
        } else if (Math.abs(other.position - user.position) == 2) {
          console.log('对家')
          classObj = 'site3'
        } else if (other.position == user.position) {
          console.log('自家')
          classObj = 'site1'
        }



        this.imgSite = classObj;
        this.imgSpcail  = type
        this.show = true;


        this.timer && clearTimeout(this.timer)
        this.timer = setTimeout(() => {
          this.imgSite = ''
          this.show = false
          this.imgSpcail  = ''
        }, 1000)
      }
    }
  }
</script>

<style scoped lang="scss">
  .con {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 1000000000;
    pointer-events: none;
    .item{
      width: 4rem;
      position: absolute;
      top: 50%;
      left: 50%;
      -webkit-transform: translate(-50%, -50%) rotate(-90deg);
      -moz-transform: translate(-50%, -50%) rotate(-90deg);
      -ms-transform: translate(-50%, -50%) rotate(-90deg);
      -o-transform: translate(-50%, -50%) rotate(-90deg);
      transform: translate(-50%, -50%) rotate(-90deg);

      &.site1{
        top:50%;
        left: 70%;
      }
      &.site2{
        top:30%;
        left: 50%;
      }
      &.site3{
        top:50%;
        left: 30%;
      }
      &.site4{
        top:70%;
        left: 50%;
      }
      img{
        width: 100%;
      }
    }

  }
</style>
